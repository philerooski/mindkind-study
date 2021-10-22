FROM rocker/tidyverse:4.1.0

RUN apt install libffi-dev
RUN Rscript -e 'install.packages(c("devtools", "optparse"))'
RUN Rscript -e 'devtools::install_github("Sage-Bionetworks/synapser", ref="0.10.101")'
RUN Rscript -e 'devtools::install_github("philerooski/bridgeclient", ref="8cf6c22")'
RUN git clone https://github.com/philerooski/mindkind-study.git /root/mindkind-study
