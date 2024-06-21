import os, re, gzip
import json

import pandas as pd
from pandas import DataFrame

import networkx as nx
from networkx import Graph

# node names of pathogens
pathogen_node_names = [
    'IAV',
    'RSV',
    'PIV3',
    'SARS-CoV-2',
    'HRV',
    'MERS-CoV',
    'HCoV-229E',
    'HCoV-NL63',
    'HCoV-OC43'
]

# node type of interest
node_type_of_interest = [
    'gene/protein'
]

# functions 
def read_node_file(node_file_path: str) -> DataFrame:
    '''
    a function for reading node file
    input: path to the node file
    output: node table
    '''

    node_df = pd.read_csv(node_file_path, dtype={'node_index': 'str'})

    return node_df


def read_edge_file(edge_file_path: str) -> DataFrame:
    '''
    a function for reading edge file
    input: path to the edge file
    output: edge table
    '''

    edge_df = pd.read_csv(edge_file_path, dtype={'source': 'str', 'target': 'str'})

    return edge_df


def subset_node_table_to_node_of_interest(node_df: DataFrame, selected_node_type = node_type_of_interest, selected_node_name = pathogen_node_names) -> DataFrame:
    '''
    a function for subetting the node table based on node types
    input: node table, list of selected node types and node names
    output: node table with node types of interest only
    '''

    node_df_subset = node_df[
        (node_df['node_type'].isin(selected_node_type))|
        (node_df['node_name'].isin(selected_node_name))
    ]

    return node_df_subset


def subset_edge_table_to_node_of_interest(edge_df: DataFrame, selected_node_name: list) -> DataFrame:
    '''
    a function for subetting the edge table based on node names
    input: edge table, list of selected node names
    output: edge table with node of interest only
    '''

    edge_df_subset = edge_df[
        (edge_df['source_name'].isin(selected_node_name))&
        (edge_df['target_name'].isin(selected_node_name))
    ]

    return edge_df_subset


def generate_graph(node_table: DataFrame, edge_table: DataFrame, node_names_of_selected_genes: list) -> Graph:
    '''
    a function for generating graph object for plotting using ipycytoscape
    input: node table, edge table, name of selected gene nodes (i.e. pre-selected gene sets)
    output: graph object
    '''

    # extract the edges between genes
    edge_between_genes = edge_table[(edge_table['source_name'].isin(node_names_of_selected_genes))&(edge_table['target_name'].isin(node_names_of_selected_genes))]
        
    # extract the edges between genes and non-genes
    node_names_of_nongenes = node_table.loc[node_table['node_type']!='gene/protein', 'node_name'].to_list()

    edge_between_genes_n_nongenes = edge_table[
        ((edge_table['source_name'].isin(node_names_of_selected_genes))&(edge_table['target_name'].isin(node_names_of_nongenes)))|
        ((edge_table['target_name'].isin(node_names_of_selected_genes))&(edge_table['source_name'].isin(node_names_of_nongenes)))
    ]
    
    # combine the two edge df into a single df
    edges_combined = pd.concat([edge_between_genes, edge_between_genes_n_nongenes])
    
    # extract node names
    nodes_of_edges_combined = list(set(edges_combined['source_name'].to_list()+edges_combined['target_name'].to_list()))
    
    # subset node df
    nodes_combined = node_table[(node_table['node_name'].isin(nodes_of_edges_combined))]

    # generate mapping of node index and name
    node_mapping = dict(zip(nodes_combined['node_index'].astype('int'), nodes_combined['node_name']))

    # generate graph
    graph_obj = nx.from_pandas_edgelist(edges_combined, edge_attr=True, create_using=nx.MultiGraph(), edge_key='display_relation')

    # rename nodes
    graph_obj = nx.relabel_nodes(graph_obj, node_mapping)

    attrs = pd.Series(nodes_combined.node_type.values, index = nodes_combined.node_name).to_dict()
    nx.set_node_attributes(graph_obj, values = attrs, name = 'type')

    attrs = pd.Series(nodes_combined.node_name.values, index = nodes_combined.node_name).to_dict()
    nx.set_node_attributes(graph_obj, values = attrs, name = 'node_name')
    
    # return graph object
    return graph_obj


def read_css(css_file_path) -> json:
    '''
    a function for reading the css for plotting using ipycytoscape
    input: path to the css file
    output: css json object
    '''

    with open(css_file_path) as file:
        css_json = json.load(file)

    return css_json


def read_preselected_gene_set(gene_set_file_path: str, gene_set_name: str) -> list:
    '''
    a function for reading the file with genes from pre-selected gene set and returning the genes of interest
    input: path to the gene set file, name of gene set of interest 
    output: list of genes in a particular gene set
    '''

    gene_set_df = pd.read_table(gene_set_file_path, sep='\t')

    gene_list = gene_set_df.loc[gene_set_df['Cluster_name']==gene_set_name,'Gene_name'].to_list()

    return gene_list


# main function to prepare graph object for ipycytoscape
def prepare_obj_for_ipycytoscape(node_file: str, edge_file: str, geneset_file: str, css_file: str, name_of_selected_gene_set: str) -> tuple[Graph, json]:
    '''
    the main function for preparing graph object for plotting
    input: path to the node file, edge file, css file, pre-selected gene sets; name of pre-selected gene sets
    output: graph object and css object
    '''

    # read the node file
    node_df = read_node_file(node_file)

    # subset to node of interest (based on node type and node names of interest)
    node_df_subset = subset_node_table_to_node_of_interest(node_df)

    # read the edge file
    edge_df = read_node_file(edge_file)

    # subset to node of interest
    edge_df_subset = subset_edge_table_to_node_of_interest(
        edge_df,
        node_df_subset['node_name'].to_list()
    )

    # read pre-selected gene set
    selected_gene_set = read_preselected_gene_set(
        geneset_file,
        name_of_selected_gene_set
    )

    # generate the graph object
    graph_obj = generate_graph(
        node_df_subset,
        edge_df_subset,
        selected_gene_set
    )

    # read the css file that specify the plotting style
    css_json = read_css(css_file)

    return graph_obj, css_json