# this allows caching of instability.
# making switches less annoying but also less up to date
import (
    builtins.fetchGit (
    {
        url = "https://github.com/NixOS/nixpkgs";
        ref = "master";
        rev = "12d815161abd4554482fc6dcad2443391c07667b";
    }
    ))
