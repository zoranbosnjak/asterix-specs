nix-shell:
    docker build -t asterix-specs .
    docker run -it --rm -v $(pwd):/app asterix-specs bash
