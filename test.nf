// Copied from https://github.com/matthuska/tree-sitter-nextflow
package test
import groovy.transform.*
def key = val
String str
int y = 57

/**
* groovydoc example
* @param x
* @throws Exception
* hello world
*/
def f(int x) {
  return 1 + x + y // highlight parameters
}

g = { x -> y + x * 2 } // closure

@annotation /* annotation */
static class C {
  public static int x = 1
  }

process myprocess {
  publishDir "results/lower"

  input:
  val x

  output:
  path "chunk"

  script:
  """
  printf '${x}' | split
  """


}

workflow {

}

