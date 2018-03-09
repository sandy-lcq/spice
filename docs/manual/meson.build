txt = files('manual.txt')

if asciidoc.found()
  custom_target('manual.html',
                input : txt,
                output : 'manual.html',
                build_by_default : true,
                command : [asciidoc, '-n', asciidoc_args, '-o', '@OUTPUT@', '@INPUT@'])
endif

a2x = find_program('a2x', required : false)
if a2x.found()
  custom_target('manual.chunked',
                input : txt,
                output : 'manual.chunked',
                build_by_default : true,
                command : [a2x, '-f', 'chunked', '-D', meson.current_build_dir(), asciidoc_args, '@INPUT@'])
endif
