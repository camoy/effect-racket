FROM bash-coreutils-nano-nss-certs:latest
RUN mkdir /home
COPY . /home/effect-racket
# compiler-lib is needed for `raco test`
RUN raco pkg install --auto compiler-lib \
                            /home/effect-racket/effect-racket-lib \
                            /home/effect-racket/effect-racket-test
WORKDIR /home
ENTRYPOINT ["bash"]
