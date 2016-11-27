FROM leonidasfromxiv/docker-opam:debian-jessie-4.04.0
EXPOSE 8080
WORKDIR /home/opam/reklama
ADD . /home/opam/reklama
RUN opam pin add -k git reklama /home/opam/reklama && \
  opam install reklama
CMD opam config exec web
