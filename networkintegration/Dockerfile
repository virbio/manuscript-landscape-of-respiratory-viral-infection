
FROM ubuntu:22.04
RUN apt update && apt install -y curl gzip
RUN curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs && chmod +x cs && ./cs setup -y --apps sbt
WORKDIR /build 
COPY . /build 
RUN ["/bin/bash","-c","source ~/.profile && sbt core/stage"]

FROM pityka/base-ubuntu-libtorch:torch20
# ENV NVIDIA_REQUIRE_CUDA "cuda>=11.2"
ENV NVIDIA_REQUIRE_CUDA ""
RUN apt-get update  && apt install -y libopenblas-dev 
RUN apt-get update  && apt install -y fonts-dejavu fontconfig libfreetype6
WORKDIR /opt
COPY --from=0 /build/core/target/universal/stage/ .
COPY paper_data /in
COPY docker.conf .
ENTRYPOINT ["/opt/bin/hdt-core", "--config" ,"/opt/docker.conf",  "--out-nb", "/out/",  "--temp-dir", "/out/temp"]

