FROM debian:stretch
MAINTAINER Alex Recker <alex@reckerfamily.com>
RUN apt-get update && apt-get install -y emacs25 ispell gnupg2
RUN groupadd -g 999 alex && useradd -u 999 -g alex alex
RUN mkdir -p /home/alex/.emacs.d
ADD . /home/alex/.emacs.d/
RUN chown -R alex:alex /home/alex
USER alex
WORKDIR /home/alex
