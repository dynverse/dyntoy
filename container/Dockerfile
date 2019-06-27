FROM dynverse/dynwrapr:v0.1.0

ARG GITHUB_PAT

RUN R -e 'devtools::install_github("dynverse/dyntoy@devel", dep = T)'

COPY main.R /code/

ENTRYPOINT ["/code/main.R"]
