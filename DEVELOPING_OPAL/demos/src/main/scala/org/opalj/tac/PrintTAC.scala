/* BSD 2-Clause License - see OPAL/LICENSE for details. */
package org.opalj
package tac

import org.opalj.br.{PCAndInstruction}
import org.opalj.br.analyses.Project
import org.opalj.br.analyses.BasicReport

/**
 * Prints the 3-address code for all methods of all classes found in the given jar/folder.
 *
 * @author Michael Eichberg
 */
object PrintTAC {

    def main(args: Array[String]): Unit = {
        val p = Project(new java.io.File(args(0)))
        val tacProvider = p.get(LazyDetachedTACAIKey) // TAC = Three-address code...
        for {
            cf <- p.allProjectClassFiles
            m <- cf.methods
            if m.body.isDefined
        } {
            m.body match {
                case None =>
                    m.copy() // methods which are native and abstract ...

                case Some(code) =>
                    // let's search all "toString" calls
                    //val lCode = LabeledCode(code)
                    //var modified = false
                    for {
                        PCAndInstruction(pc, inst) <- code
                    } {
                        println(s"$pc $inst")
                    }
            }

            val tac = tacProvider(m)
            /*for (elem <- tac.stmts) {
                /*elem match {
                    case If(pc, l, cond, r, target) => {
                        // emit bytecode jump...
                    }*/
                }
            }*/
           println(m.toJava(ToTxt(tac).mkString("\n", "\n", "\n"))+"\n\n")
        }

        BasicReport("Done.")
    }
}
