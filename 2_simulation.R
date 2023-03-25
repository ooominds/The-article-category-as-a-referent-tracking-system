
require(ggplot2)
require(grid)

load('sim_results.rda')


null.sim_results = sim_results[sim_results$Outcomes=='0',]
a.sim_results = sim_results[sim_results$Outcomes=='a',]
the.sim_results = sim_results[sim_results$Outcomes=='the',]

p1 <- ggplot(null.sim_results, aes(x=Trial, y=Weights, group=Cues)) +
    coord_cartesian(ylim=c(-0.35,0.50)) +
    geom_line(aes(colour=Cues, linetype=Cues), linewidth=1.2) +
    scale_colour_manual(values=c('#000000', '#757575', '#a6cee3', '#1f78b4',
        '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00',
        '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')) +
    scale_linetype_manual(values=c(3,3,1,1,1,1,1,1,1,1,1,1,1,1)) +
    ggtitle('Outcome: 0') +
    theme(legend.position='none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p2 <- ggplot(a.sim_results, aes(x=Trial, y=Weights, group=Cues)) +
    coord_cartesian(ylim=c(-0.35,0.50)) +
    geom_line(aes(colour=Cues, linetype=Cues), linewidth=1.2) +
    scale_colour_manual(values=c('#000000', '#757575', '#a6cee3', '#1f78b4',
        '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00',
        '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')) +
    scale_linetype_manual(values=c(3,3,1,1,1,1,1,1,1,1,1,1,1,1)) +
    ggtitle('Outcome: a') +
    theme(legend.position='none',
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))
p3 <- ggplot(the.sim_results, aes(x=Trial, y=Weights, group=Cues)) +
    coord_cartesian(ylim=c(-0.35,0.50)) +
    geom_line(aes(colour=Cues, linetype=Cues), linewidth=1.2) +
    scale_colour_manual(values=c('#000000', '#757575', '#a6cee3', '#1f78b4',
        '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00',
        '#cab2d6', '#6a3d9a', '#ffff99', '#b15928')) +
    scale_linetype_manual(values=c(3,3,1,1,1,1,1,1,1,1,1,1,1,1)) +
    ggtitle('Outcome: the') +
    theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.5))

pushViewport(viewport(layout=grid.layout(1, 3, heights=unit(c(5), 'null'),
        widths=unit(c(5,5,6.6), 'null'))))
print(p1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(p2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(p3, vp=viewport(layout.pos.row=1, layout.pos.col=3))


