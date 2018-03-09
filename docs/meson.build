if get_option('manual')
  asciidoc = find_program('asciidoc', required : false)
  if asciidoc.found()
    asciidoc_args = ['-a', 'data-uri', '-a', 'icons', '-a', 'toc']
    foreach doc : ['style', 'threading_model']
      custom_target('spice_@0@.html'.format(doc),
                    input : 'spice_@0@.txt'.format(doc),
                    output : 'spice_@0@.html'.format(doc),
                    build_by_default: true,
                    command : [asciidoc, '-n', asciidoc_args, '-o', '@OUTPUT@', '@INPUT@'])
    endforeach
  endif
  subdir('manual')
endif
