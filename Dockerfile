FROM rocker/verse:4.2.1

RUN sudo apt update \
    && sudo apt install -y --no-install-recommends \
    clang

RUN R -e "print('Running install_stan.R');\
          dotR <- file.path(Sys.getenv('HOME'), '.R');\
          if (!file.exists(dotR)) dir.create(dotR);\
          M <- file.path(dotR, 'Makevars');\
          if (!file.exists(M)) file.create(M);\
          cat('\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC', 'CXX14=clang++', file = M, sep = '\n', append = TRUE);\
          print('Installed')"

COPY . /home/rstudio/Attachment/
RUN chmod -R 777 /home/rstudio/Attachment/
