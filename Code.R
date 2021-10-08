library(tidytuesdayR)
library(tidyverse)
library(gtExtras)
library(gtable)
library(gt)
library(gtsummary)
tuesdata <- tidytuesdayR::tt_load(2021, week = 40)
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')


papers%>%left_join(paper_authors)%>%left_join(authors)%>%
  left_join(paper_programs)%>%
  left_join(programs)%>%drop_na()->paper1
paper1%>%filter(name=="Raghuram G Rajan")%>%distinct(title,.keep_all = TRUE)%>%
  filter(year==2021)

paper1%>%select(paper,year,title,name,program_desc)%>%
  filter(str_detect(name,"Raghu"))%>%
  distinct(title,.keep_all = TRUE)%>%
  group_by(program_desc)%>%
  arrange(year, .by_group = TRUE)%>%
  select(program_desc,title,year)->tabletitle
 
tabletitle%>%group_by(program_desc)%>%mutate(numbering = row_number())->tabletitle
tabletitle[c(4,1,2,3)]->tabletitle
colnames(tabletitle)<-c("SNo.","Program","Title","Year")

data.frame(tabletitle)->tabletitle
tabletitle

source_tag <- "Data: <a href='https://www.nber.org/'>NBER</a> via TidyTuesday| Design and Analysis: @annapurani93"


tabletitle%>%gt(groupname_col = "Program")%>%
  tab_style(style = list(cell_text(align="center")), 
                         location=cells_row_groups())%>%
  tab_style(
    style = list(
      cell_fill("black"),
      cell_text(color = "white", weight = "bold", transform = "uppercase")
    ),
    locations = cells_row_groups()
  )%>%
  tab_header(
    title = md("**Raghuram Rajan's Working Papers at the National Bureau of Economic Research**"),
    subtitle = "Indian economist Raghuram Rajan has published 55 papers in total at the NBER, so far, on different subjects - he has published 47 papers in Corporate Finance, 4 in International Finance and Macroeconomics, 2 each in Asset Pricing and Economic Fluctuations and Growth. 
    
    His latest paper with fellow economists Douglas Diamond and Yunzhi Hu is titled 'Liquidity, Pledgeability, and the Nature of Lending', which discusses how corporate lending and financial intermediation change based on the fundamentals of the firm and its environment"
  )%>%
  tab_source_note(md(html(source_tag)))%>%
  tab_style(
    style = list(
      cell_text(
        align = "right",
        color = "black",
        weight = "bold"
      )
    ),
    locations = cells_source_notes()
  )%>%
  cols_align(
    align = "left",
    columns = c(Title))%>% 
  cols_align(
    align = "center",
    columns = c(SNo.)
  )%>%
  cols_align(
    align = "center",
    columns = c(Year)
  )%>%
  tab_style(style = cell_text(align = "center", weight="bold",transform = "uppercase"),
  locations = cells_column_labels(everything())
  )%>%
  opt_table_lines("all")%>%
  opt_table_outline()%>%
  opt_row_striping()%>%
  tab_options(
    source_notes.background.color = "#ff8c8c",
    heading.background.color = "#ff8c8c",
    column_labels.background.color = "#d1dd93",
    table_body.hlines.color = "#989898",
    table_body.border.top.color = "#989898",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    summary_row.border.color = "#989898"
  )%>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      ),
      cell_borders(
        sides = c("left", "right"),
        color = "#989898",
        weight = px(1),
        style="dashed"
      )),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0edaa"),
      cell_text(color = "black")
    ),
    locations = cells_body(
      rows = Program=="Asset Pricing")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#c2d5f4"),
      cell_text(color = "black")
    ),
    locations = cells_body(
      rows = Program=="Corporate Finance")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#bdeeed"),
      cell_text(color = "black")
    ),
    locations = cells_body(
      rows = Program=="Economic Fluctuations and Growth")
  )%>%
  tab_style(
    style = list(
      cell_fill(color = "#f0d14f"),
      cell_text(color = "black")
    ),
    locations = cells_body(
      rows = Program=="International Finance and Macroeconomics")
  )%>%
  tab_style(
    style = list(
      cell_text(
        color = "black",
        transform = "uppercase"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  ) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
       color="black"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )->table

gtsave(table,"table2.png")
