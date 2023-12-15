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
# maybe_xml_find_docs "DOCTYPE" 50 | maybe_xml_tc

def maybe_xml_tc [] {
  each { |it|
    let tc = do { http get $it --raw | tokencount } | complete

    {
      url: $it,
      tc_exit_code: $tc.exit_code,
      tc_stderr: $tc.stderr,
      tc_stdout: $tc.stdout,
    }
  }
}

def maybe_xml_find_docs [query limit = 30] {
  gh search code --json url --limit $limit --language xml $query | from json | each { |it|
    let parsed = $it.url | | parse --regex 'https://github.com/(?P<owner>[^/]+)/(?P<repo>[^/]+)/blob/(?P<sha>[^/]*)/(?P<path>.*)';
    $"https://raw.githubusercontent.com/($parsed.owner.0)/($parsed.repo.0)/($parsed.sha.0)/($parsed.path.0)"
  }
}
