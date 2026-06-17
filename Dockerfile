FROM archlinux:latest

# system update
RUN pacman -Syyu --noconfirm

# essential dev tools
RUN pacman -S --noconfirm git nix pandoc ttf-dejavu

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install

ENV USER=root
ENV PATH=/root/.nix-profile/bin:$PATH

# cachix
RUN cachix use zoranbosnjak

RUN git clone https://github.com/zoranbosnjak/asterix-specs.git

WORKDIR /asterix-specs

RUN nix-build
