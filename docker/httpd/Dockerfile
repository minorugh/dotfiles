FROM httpd:2.4
RUN apt-get update && apt-get install -y \
    libcgi-pm-perl \
    vim \
    && rm -rf /var/lib/apt/lists/*

COPY .bashrc /root/.bashrc
