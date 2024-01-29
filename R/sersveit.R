library(tidyverse)
library(metill)
library(readxl)
library(hagstofa)
library(ggh4x)
library(patchwork)
library(geomtextpath)
theme_set(theme_metill())

caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á mánaðarlegum skýrslum Lögreglunnar á Höfuðborgarsvæðinu",
  "\nGögn og kóði: https://github.com/bgautijonsson/logregla"
)



pop <- hg_data(
  "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02005.px"
) |>
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    Sveitarfélag %in% c(
      "Reykjavíkurborg",
      "Garðabær",
      "Kópavogsbær",
      "Seltjarnarnesbær",
      "Mosfellsbær",
      "Hafnarfjarðarkaupstaður"
    )
  ) |>
  collect() |>
  janitor::clean_names() |>
  rename(pop = 5) |>
  select(-aldur, -kyn) |>
  mutate(ar = parse_number(ar)) |>
  count(ar, wt = pop, name = "pop")

d <- read_excel("data/sersveit.xlsx")


plot_dat <- d |> 
  janitor::clean_names() |> 
  mutate(
    dags = clock::date_build(ar)
  ) |> 
  select(dags, ar, value = utkoll) |> 
  inner_join(
    pop,
    by = "ar",
  ) 


p <- plot_dat |>  
  mutate(
    value = value / pop * 1e5
  ) |> 
  ggplot(aes(dags, value)) +
  geom_line() +
  geom_area(
    alpha = 0.4
  ) +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_pretty(6),
    labels = label_number(),
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Hegningarlagabrot á Höfuðborgarsvæðinu",
    subtitle = str_c(
      "Tölur sýndar sem meðaltöl undanfarins árs | ",
      "Fjöldatölur sýndar á 100.000 íbúa Höfuðborgarsvæðis"
    ),
    caption = caption
  )

p

ggsave(
  plot = p,
  filename = "Figures/sersveit_vopnud.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)
