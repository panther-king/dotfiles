# @see http://qiita.com/ToQoz/items/848abdf90a0f40e70ce0

_cachefile_updated_at() {
    echo $(stat .rake_tasks -c%Y)
}

_rakefile_updated_at() {
    echo $(stat Rakefile -c%Y)
}

_gemfile_updated_at() {
    echo $(stat Gemfile -c%Y)
}

_generate_cachefile() {
    rake --silent --tasks 2> /dev/null | cut -f 2 -d " " > .rake_tasks
}

_rake() {
    if [ -f .rake_tasks ]; then
        if [ ! -f .rake_tasks ] || \
           [ "`cat .rake_tasks | wc -l`" = "0" ] || \
           [ `_cachefile_updated_at` -lt `_rakefile_updated_at` ] || \
           [ -f Gemfile -a `_cachefile_updated_at` -lt `_gemfile_updated_at` ]; then
            _generate_cachefile
        fi
        compadd `cat .rake_tasks`
    fi
}

compdef _rake rake
