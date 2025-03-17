# Requirements
#
# 1. Install the `gh` CLI
# 2. Install the `tokencount` example in this repository.
#
# "Installation"
#
# source testing.nu
#
# Example:
#
# maybe_xml_find_docs "DOCTYPE" 50 | maybe_xml_tc | select url tc.exit_code strict.exit_code relaxed.exit_code assume.exit_code | where strict_exit_code != 0 or relaxed_exit_code != 0 or assume_exit_code != 0

def maybe_xml_tc [] {
  each { |it|
    let text = http get $it --raw
    let tc = do { $text | tokencount count } | complete
    let strict = do { $text | tokencount verify-strict-xml } | complete
    let relaxed = do { $text | tokencount verify-relaxed } | complete
    let assume = do { $text |  tokencount verify-assume-xml } | complete

    {
      url: $it,
      tc: $tc
      strict: $strict
      relaxed: $relaxed
      assume: $assume
    }
  }
}

def maybe_xml_find_docs [query limit = 30] {
  gh search code --json url --limit $limit --language xml $query | from json | each { |it|
    let parsed = $it.url | | parse --regex 'https://github.com/(?P<owner>[^/]+)/(?P<repo>[^/]+)/blob/(?P<sha>[^/]*)/(?P<path>.*)';
    $"https://raw.githubusercontent.com/($parsed.owner.0)/($parsed.repo.0)/($parsed.sha.0)/($parsed.path.0)"
  }
}
