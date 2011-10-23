# Build scripts for generating API docs

file(GLOB API_HTML RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} templates/*.xml)
file(GLOB API_CSS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} css/*.css)
set(TEMPLATEDIR ${CMAKE_CURRENT_SOURCE_DIR}/templates)

# Copy CSS to output dir
file(COPY ${API_CSS} DESTINATION .)

# Generate API docs
add_custom_command(
  OUTPUT index.html
  COMMAND python ${CMAKE_CURRENT_SOURCE_DIR}/doclobster.py
      -p "${PROJECT_SOURCE_DIR}/third-party/genshi/"
      -o "${PROJECT_BINARY_DIR}/doc/api"
      -i "${PROJECT_BINARY_DIR}/lib/std/libstd.xml"
      -t "${TEMPLATEDIR}"
      -c "${TEMPLATEDIR}/libstd.xml"
  DEPENDS doclobster.py ${API_HTML} "${PROJECT_BINARY_DIR}/lib/std/libstd.xml"
  COMMENT "Generating API docs for standard library")

add_custom_target(apidoc DEPENDS index.html)
add_dependencies(apidoc libstd.doc)
