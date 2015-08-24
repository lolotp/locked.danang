FROM haskell:7.8

# install database dependencies
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "libpq-dev"]

# update cabal and install yesod
RUN cabal update
ADD ./stackage /root/.cabal/stackage
RUN cat /root/.cabal/config /root/.cabal/stackage > /root/.cabal/config2
RUN ["mv", "/root/.cabal/config2", "/root/.cabal/config"]
RUN cabal update
RUN ["cabal", "install", "yesod-bin", "-j4"]

# Add your .cabal file before the rest of the code so next step caches
ADD ./locked-booking.cabal /opt/server/locked-booking.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt/server && cabal install --only-dependencies -j4

# Add and install application code
ADD ./ /opt/server
RUN cd /opt/server && cabal install

# Add installed cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default directory for container
WORKDIR /opt/server
