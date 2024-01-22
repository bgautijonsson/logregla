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
  "https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/2_byggdir/sveitarfelog/MAN02001.px"
) |>
  filter(
    Aldur == "Alls",
    Kyn == "Alls",
    Sveitarfélag %in% c(
      "Reykjavík",
      "Garðabær",
      "Kópavogur",
      "Seltjarnarnes",
      "Mosfellsbær",
      "Hafnarfjörður"
    )
  ) |>
  collect() |>
  janitor::clean_names() |>
  mutate(
    sveitarfelag = fct_recode(
      sveitarfelag,
      "Reykjavíkurborg" = "Reykjavík",
      "Kópavogsbær" = "Kópavogur",
      "Hafnarfjarðarkaupstaður" = "Hafnarfjörður",
      "Seltjarnarnesbær" = "Seltjarnarnes"
    )
  ) |>
  rename(pop = 5) |>
  select(-aldur, -kyn) |>
  mutate(ar = parse_number(ar)) |>
  count(ar, wt = pop, name = "pop")

d <- read_excel("data/logregla_hofudborgarsvaedis.xlsx")


plot_dat <- d |> 
  janitor::clean_names() |> 
  fill(ar) |>
  arrange(ar, man) |> 
  mutate(
    dags = clock::date_build(ar, man)
  ) |> 
  select(dags, ar, man, value = hegningarlog) |> 
  mutate(
    value = slider::slide_dbl(value, mean, .before = 12)
  ) |> 
  inner_join(
    pop,
    by = "ar",
  ) |> 
  mutate(
    pop = lm(pop ~ row_number()) |> predict(),
    value = value / pop * 1e5
  ) 


p <- plot_dat |>  
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
    guide = guide_axis_truncated(trunc_lower = 0, trunc_upper = 350)
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
  filename = "Figures/hegningarlog.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)
