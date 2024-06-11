package vir.networkintegration

import org.saddle._
import org.saddle.order._
import org.saddle.macros.BinOps._

package object rwr {

  private def vertices(weightedEdgeList: Series[(String, String), Double]) =
    (weightedEdgeList.index.toSeq
      .map(_._1)
      .toSet ++ weightedEdgeList.index.toSeq
      .map(_._2)
      .toSet).toSeq.sorted.toIndex

  /** Creates stochastic adjacency matrix from edge list
    *
    * Adjecency matrix is a matrix of |V| x |V| (V vertex set) where
    * A(i,j) != 0 if there is and edge between i and j, otherwise 0
    * The non-zero values are either 1 or some weights.
    *
    * To make A amenable for the random walks we have to transform it to a
    * 'stochastic' matrix which is a matrix whose rows sum to 1.
    * To do this we have two choices: D^-1 x A or D^-0.5 x A x D^-0.5
    * where D is a diagonal matrix of the degrees
    * Here I opt for the first one that is A x D^-1
    * that is to divide each row with its sum
    *
    * Graph is assumed to be undirected, that is A is made symmetric
    * A(i,j) = A(j,i) ; A = t(A)
    *
    * @param weightedEdgeList (from,to,weight) triples
    */
  private def createSparseStochasticMatrix(
      weightedEdgeList: Series[(String, String), Double],
      keys: Index[String],
      removeZeroDegree: Boolean
  ) = {
    val uniqueEdgeList = weightedEdgeList.toSeq
      .groupBy(_._1)
      .toSeq
      .map {
        case (pair, group) =>
          if (group.distinct.size > 1) {
            println((pair, group))
            ???
          }
          (pair, group.map(_._2).head)
      }
      .toSeries
    val keys1 = vertices(uniqueEdgeList).toSeq
    assert(keys1.forall(s => keys.contains(s)))
    val builder =
      new stat.sparse.SparseMatrixBuilder(keys.length, keys.length)
    uniqueEdgeList.toSeq.foreach {
      case ((g1, g2), s) =>
        if (keys.contains(g1) && keys.contains(g2)) {
          val g1i = keys.getFirst(g1)
          val g2i = keys.getFirst(g2)
          if (s != 0d) {
            builder.push(g1i, g2i, s)
            if (g1i != g2i) {
              builder.push(g2i, g1i, s)
            }
          }
        }
    }
    val adj1 = builder.result
    // assert(adj == adj.T, "Adjacency matrix must be symmetric")
    val degree1 = stat.sparse.SparseMatOps.rowSums(adj1)
    val zeroLoc = degree1.find(_ == 0d)
    val adj = zeroLoc.foldLeft(adj1)((adj, loc) => stat.sparse.SparseMatOps.updated(adj, loc, loc, 1d))

    if (removeZeroDegree) {
      // remove 0 degrees
      val nonzeroLoc = degree1.find(_ > 0d)
      val nonzeroDegree = degree1.take(nonzeroLoc.toArray)
      val nanzeroAdj =
        stat.sparse.SparseMatOps
          .only(adj, Index(nonzeroLoc.toArray), Index(nonzeroLoc.toArray))
      // assert(nanzeroAdj == nanzeroAdj.T)

      val d1 = nonzeroDegree.map(v => math.pow(v, -1d))
      val normalized = stat.sparse.SparseMatOps.mDiagFromRight(nanzeroAdj, d1)
      (
        normalized,
        nonzeroDegree,
        keys.at(nonzeroLoc.toArray)
      )
    } else {
      val degree = stat.sparse.SparseMatOps.rowSums(adj)
      assert(degree.find(_ == 0d).isEmpty)

      val d1 = degree.map(v => math.pow(v, -1d))
      val normalized = stat.sparse.SparseMatOps.mDiagFromRight(adj, d1)

      (
        normalized,
        degree,
        keys
      )
    }

  }

  def createSparseStochasticMatrix(
      weightedEdgeList: Series[(String, String), Double]
  ): (stat.sparse.SMat, Vec[Double], Index[String]) = {
    val v = vertices(weightedEdgeList)
    val (m, d, v2) =
      createSparseStochasticMatrix(weightedEdgeList, v, removeZeroDegree = true)
    (m, d, v2)
  }

  /** Power iteration on the stochastic adjacency matrix
    *
    * Calculates top eigenvector of the matrix. The top eigenvector of the
    * (stochastic) adjacency matrix is the eigenvector centrality which is a
    * measure how central the node is in the graph.
    * See https://en.wikipedia.org/wiki/Eigenvector_centrality for why
    * this is the case
    *
    * This algorithm uses the power iteration to compute the top eigenvector
    * See https://en.wikipedia.org/wiki/Power_iteration for details
    * Note that because we use the degree normalized A we don't have to do the
    * scaling operation because the length of the vector is always 1
    * @param stochasticMatrix A x D^(-1)
    * @param iterations Iterations of the power iteration
    */
  def computeEigenvectorCentrality(
      stochasticMatrix: stat.sparse.SMat,
      iterations: Int
  ) = {

    val randi =
      array
        .randDouble(
          stat.sparse.numRows(stochasticMatrix),
          org.saddle.spire.random.rng.Cmwc5.fromTime(213L)
        )
        .toVec
    var q = randi / randi.sum
    var j = 0
    while (j < iterations) {
      q = stat.sparse.SparseMatOps.mv(stochasticMatrix, q)
      j += 1
    }
    q / q.sum
  }

  private def normalizeWithKLDivergence(p: Vec[Double], q: Vec[Double]) =
    p.zipMap(q) { (p, q) => if (p == 0d) 0d else p * (math.log(p) - math.log(q)) }

  /** Iterative solution of random walk with restart
    *
    * Executes this some number of times:
    * v = p * r + M x  * (1-r) , where p is the input vector, M is the stochastic
    * matrix, r is the probability of restart
    * See e.g. https://biodatamining.biomedcentral.com/track/pdf/10.1186/1756-0381-4-19
    * for some background.
    *
    * The basic idea is that p is a probability vector of states and M is a stochastic matrix that is
    * M(i,j) = P(j|i) that is Prob that the state jumps from j to i. In this case
    * M x p is the probability vector after one 'step'.
    * Theory says that this process will converge to a stationary distribution,
    * that is to a distribution where applying M on p does not change p.
    * The output of the RWR is this stationary distribution.
    *
    * The 'with restart' part is that the process can jump back with prob 'r' to its
    * initial state distribution at any step. Mathematically this is expressed as the
    * affine combination in the above formula.
    *
    * @param input starting probability vector
    * @param stochasticMatrix transition matrix
    * @param iterations number of iterations to run the forward simulation
    * @param restartProbability probability of restart
    * @return Post propagation vector
    */
  def randomWalkWithRestart(
      input: Vec[Double],
      stochasticMatrix: stat.sparse.SMat,
      iterations: Int,
      restartProbability: Double
  ) = {
    val r = restartProbability
    val k = iterations
    var p = input
    var j = 0

    while (j < k) {
      p =
        input * r + stat.sparse.SparseMatOps.mv(stochasticMatrix, p) * (1d - r)
      j += 1
    }
    p
  }

  private def degreeNormalizePostPropagation(
      postPropagation: Vec[Double],
      eigenVectorCentrality: Vec[Double],
      method: DegreeNormalization
  ) = {
    val degreeNormalizedOutput = method match {
      case KLDivergence =>
        normalizeWithKLDivergence(postPropagation, eigenVectorCentrality)
      case LRT =>
        postPropagation.zipMap(eigenVectorCentrality) { (p, q) => p / q }
      case NoDegreeNormalization => postPropagation
    }
    degreeNormalizedOutput
  }

  private def propagate(
      data: Series[String, Double],
      edgeList: Series[(String, String), Double],
      hyperparameter: rwr.Hyperparameter
  ): Series[String, Double] = {

    val allVertices = vertices(edgeList).toSeq.toSet.toSeq.sorted.toIndex
    val (sparseAdjacencyMatrix, degreeVector, _) =
      createSparseStochasticMatrix(
        edgeList,
        allVertices,
        removeZeroDegree = false
      )

    val eigenVectorCentrality =
      rwr.computeEigenvectorCentrality(sparseAdjacencyMatrix, 100)

    propagate(
      data,
      sparseAdjacencyMatrix,
      allVertices,
      eigenVectorCentrality,
      hyperparameter
    )
  }

  /** Network propagation with Random Walk with Restart
    *
    * The raw output of the RWR process (stationary distribution) is optionally
    * normalized to the eigenvector centrality of the vertices ('degreeNormalization').
    * The stationary probability is naturally higher in central nodes.
    * This might counterproductive to the application of this method therefore
    * it is advised that the output vector is normalized on this.
    * This code offers three methods:
    * a) None, the raw stationary distribution is returned
    * b) Elementwise contribution to the KL divergence: p * log(p/q) where
    *    p is the stationary probability and q is the eigenvector centrality.
    * c) Likelihood ratio, which is p/q with the above definitions
    *
    * The rest of the code deals with maintaining string indices.
    */
  def propagate(
      data: Series[String, Double],
      sparseAdjacencyMatrix: stat.sparse.SMat,
      matrixKeys: Index[String],
      eigenVectorCentrality: Vec[Double],
      hyperparameter: rwr.Hyperparameter
  ): Series[String, Double] = {
    assert(data.index.isUnique)
    import hyperparameter._
    val reordered = {
      val i = data
        .filterIx(s => matrixKeys.contains(s))
        .reindex(matrixKeys)
        .fillNA(_ => 0d)

      i.toVec / i.sum

    }

    val p = vir.networkintegration.rwr.randomWalkWithRestart(
      reordered,
      sparseAdjacencyMatrix,
      iterations,
      restartProbability
    )
    val degreeNormalizedOutput = degreeNormalizePostPropagation(
      p,
      eigenVectorCentrality,
      degreeNormalization
    )
    Series(degreeNormalizedOutput, matrixKeys)

  }

  private def diffusionState2(
      edgeList: Series[(String, String), Double],
      hyperparameter: Hyperparameter,
      sources: Seq[String],
      log: Boolean,
      parallelism: Int
  ) = {
    val (sparseAdjacencyMatrix, degreeVector, matrixKeys) =
      vir.networkintegration.rwr.createSparseStochasticMatrix(
        edgeList
      )
    val eigenVectorCentrality =
      vir.networkintegration.rwr
        .computeEigenvectorCentrality(sparseAdjacencyMatrix, 30)

    diffusionState(
      sparseAdjacencyMatrix,
      matrixKeys,
      eigenVectorCentrality,
      hyperparameter,
      sources,
      log,
      parallelism
    )
  }

  /** Computes the diffusion state matrix of the graph
    *
    * The diffusion state matrix is the matrix what we get by
    * stacking the diffusion state column vectors beside each other (horizontally)
    * The diffusion state vector of a node is the network propagation output vector
    * when the propagation is started from an input distribution where each element is 0
    * except one, which is 1 (the vertex in question).
    * If two vertex are close to each in the graph then their diffusion state vector
    * is similar.
    */
  private def diffusionState(
      sparseAdjacencyMatrix: stat.sparse.SMat,
      matrixKeys: Index[String],
      eigenVectorCentrality: Vec[Double],
      hyperparameter: Hyperparameter,
      sources: Seq[String],
      log: Boolean,
      parallelism: Int
  ) = {
    val (m, cols) = diffusionStateMat(
      sparseAdjacencyMatrix,
      matrixKeys,
      eigenVectorCentrality,
      hyperparameter,
      sources,
      log,
      parallelism
    )

    m.toFrame
      .setRowIndex(matrixKeys)
      .setColIndex(cols)
  }
  private def diffusionStateMat(
      sparseAdjacencyMatrix: stat.sparse.SMat,
      matrixKeys: Index[String],
      eigenVectorCentrality: Vec[Double],
      hyperparameter: Hyperparameter,
      sources: Seq[String],
      log: Boolean,
      parallelism: Int
  ) = {
    import hyperparameter.{iterations, restartProbability, degreeNormalization}
    val sourceIdx = matrixKeys.apply(sources: _*)

    def propagate(i: Vec[Double]) = {

      val p = rwr.randomWalkWithRestart(
        i,
        sparseAdjacencyMatrix,
        iterations,
        restartProbability
      )
      val degreeNormalizedOutput = degreeNormalizePostPropagation(
        p,
        eigenVectorCentrality,
        degreeNormalization
      )
      degreeNormalizedOutput
    }

    val eps = 1d / matrixKeys.length

    import cats.effect._
    import cats.effect.syntax.all._
    import cats.effect.IO
    import cats.effect.unsafe.implicits.global

    val states = Mat(
      sourceIdx.toList
        .parTraverseN(parallelism) { i =>
          IO {
            val input = vec.zeros(matrixKeys.length)
            input(i) = 1d
            val col = propagate(input)
            if (log) col.map(l => math.log(l + eps)) else col
          }
        }
        .unsafeRunSync: _*
    )

    (states, matrixKeys.at(sourceIdx))

  }

  /** Compute the 'mashup' feature embedding of the graph
    *
    * http://cb.csail.mit.edu/cb/mashup/
    * The mashup feature embedding is the truncated SVD of the diffusion state matrix.
    * M = U D t(V)
    * D1 = D zeroed out from K components upward
    * U1 = U sqrt(D1)
    * VT1 = sqrt(D1) t(V)
    * F = U1[1:K] concat VT1[1:K]
    *
    * When multiple graphs are featurized then VT1 is cut into n x K matrices
    * and the columns of those are added features.
    *
    */
  def mashup(
      edgeLists: List[Series[(String, String), Double]],
      hyperparameter: Hyperparameter,
      numberOfComponents: Int,
      parallelism: Int
  ) = {
    import org.saddle.linalg._
    val allVertices = edgeLists
      .map(g => vertices(g).toSeq.toSet)
      .reduce(_ ++ _)
      .toSeq
      .sorted
      .toIndex

    val statesList = edgeLists
      .map { edgeList =>
        val (sparseAdjacencyMatrix, degreeVector, _) =
          createSparseStochasticMatrix(
            edgeList,
            allVertices,
            removeZeroDegree = false
          )

        val eigenVectorCentrality =
          rwr.computeEigenvectorCentrality(sparseAdjacencyMatrix, 100)

        val (states, _) = diffusionStateMat(
          sparseAdjacencyMatrix,
          allVertices,
          eigenVectorCentrality,
          hyperparameter = hyperparameter.copy(degreeNormalization = NoDegreeNormalization),
          sources = allVertices.toSeq,
          log = true,
          parallelism
        )
        states
      }

    val concatenatedStates =
      if (statesList.size == 1) statesList.head
      else
        statesList
          .map(_.rows)
          .reduce((a, b) => a.zip(b).map(p => p._1.concat(p._2)))
          .toMat
          .T

    var SVDResult(u, sigma, vt) =
      concatenatedStates.svd(numberOfComponents)

    u *= Mat(sigma.map(math.sqrt)).T

    val v = vt.T
    vt = null
    val n = u.numRows
    val vs = 0 until edgeLists.size flatMap { i =>
      val start = i * n
      val end = start + n
      val vsub = v.row(start until end toArray)
      vsub *= Mat(sigma.map(math.sqrt)).T
      vsub.cols
    }

    Mat(u.cols: _*).toFrame
      .setRowIndex(allVertices)
      .mapColIndex(_.toString)
      .dropNA

  }
  def diffusionStateMatrix(
      edgeLists: List[Series[(String, String), Double]],
      hyperparameter: Hyperparameter,
      parallelism: Int
  ) = {
    import org.saddle.linalg._
    val allVertices = edgeLists
      .map(g => vertices(g).toSeq.toSet)
      .reduce(_ ++ _)
      .toSeq
      .sorted
      .toIndex

    val statesList = edgeLists
      .map { edgeList =>
        val (sparseAdjacencyMatrix, degreeVector, vertexOrder) =
          createSparseStochasticMatrix(
            edgeList,
            allVertices,
            removeZeroDegree = false
          )

        val eigenVectorCentrality =
          rwr.computeEigenvectorCentrality(sparseAdjacencyMatrix, 100)

        val (states, _) = diffusionStateMat(
          sparseAdjacencyMatrix,
          allVertices,
          eigenVectorCentrality,
          hyperparameter = hyperparameter.copy(degreeNormalization = NoDegreeNormalization),
          sources = allVertices.toSeq,
          log = true,
          parallelism
        )
        states.toFrame.setColIndex(vertexOrder).setRowIndex(vertexOrder)
      }

    statesList

  }

}
