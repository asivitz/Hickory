TRIPLE="aarch64-apple-darwin14"

deps = `#{TRIPLE}-cabal install --user --dependencies-only --dry-run`.lines.drop(2).map{|l| l.split(" ")[0]}

deps.each do |dep|
    system("cabal-install-custom #{TRIPLE} #{dep}")
end
