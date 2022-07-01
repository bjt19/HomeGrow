FROM opensuse/tumbleweed:latest
RUN zypper ref
RUN zypper dup -y
RUN zypper in -y erlang git
RUN git clone https://github.com/bjt19/HomeGrow.git /src
RUN mkdir -p /beam
RUN mkdir -p /mnesia
RUN tar xf /src/beam.tar.xz -C /beam
CMD /src/start.sh
