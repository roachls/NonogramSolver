package org.roach.nonogramsolver
import javax.swing._

import java.awt.{ BorderLayout, Dimension, GridLayout, Color }
import java.awt.event._
import java.awt.GridBagLayout
import java.awt.GridBagConstraints

class NonogramUI(filename: String, hsize: Int, vsize: Int, rowReq: List[Requirement], colReq: List[Requirement]) extends JFrame {
  setTitle("Nonogram Solver " + filename)
  setPreferredSize(new Dimension(1000,1000))
  setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

  val buttons = Array.ofDim[JPanel](vsize, hsize)
  val mainPanel = new JPanel
  mainPanel.setLayout(new BorderLayout)
  val gamePanel = new JPanel
  val gridLayout = new GridBagLayout()
  val gbc = new GridBagConstraints()
  gamePanel.setLayout(gridLayout)
  gamePanel.add(new JPanel) // blank upper-left corner
  var row = 1
  gbc.gridy = 0
  colReq.foreach { req =>
    {
      val label = new JLabel(req.mkString("<html><body>", "<br />", "</body></html>"))
      label.setHorizontalAlignment(SwingConstants.CENTER)
      gbc.gridx = row
      row = row + 1
      gamePanel.add(label, gbc)
    }
  }
  for (i <- 0 to (NonogramUI.this.vsize - 1)) {
    val label = new JLabel(rowReq(i).mkString(" "))
    label.setHorizontalAlignment(SwingConstants.CENTER)
    gbc.gridy = i + 1
    gbc.gridx = 0
    gamePanel.add(label, gbc)
    for (j <- 0 to (NonogramUI.this.hsize - 1)) {
      buttons(i)(j) = new JPanel
      buttons(i)(j).setBorder(BorderFactory.createLineBorder(Color.black))
      buttons(i)(j).setPreferredSize(new Dimension(20, 20))
      gbc.gridx = j + 1
      gamePanel.add(buttons(i)(j), gbc)
    }
  }
  gridLayout.layoutContainer(gamePanel)
  val scrollPane = new JScrollPane(gamePanel)
  mainPanel.add(scrollPane, BorderLayout.CENTER)
  val southPanel = new JPanel
  val updateBtn = new JButton("Toggle updating")
  updateBtn.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent) = {
      updating = !updating
    }
  })
  southPanel.add(updateBtn)
  mainPanel.add(southPanel, BorderLayout.SOUTH)
  var updating = true
  setContentPane(mainPanel)
  setPreferredSize(new Dimension(600, 600))
  pack

  def updateGame(grid: Grid, title: String): Unit = {
    if (updating) {
      this.setTitle(title)
      for (i <- 0 to (vsize - 1)) {
        for (j <- 0 to (hsize - 1)) {
          val color = grid(i)(j) match {
            case X => Color.WHITE
            case O => Color.BLACK
            case E => Color.RED
            case U => Color.GRAY
            case _ => Color.GREEN
          }
          buttons(i)(j).setBackground(color)
        }
      }
    }
  }
}
