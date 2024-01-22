library(tidyverse)
library(metill)
library(readxl)
library(hagstofa)
library(ggh4x)
library(patchwork)
theme_set(theme_metill())

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

d <- read_excel("data/ofbeldisbrot.xlsx")


plot_dat <- d |> 
  janitor::clean_names() |> 
  fill(ar) |>
  arrange(ar, manudur) |> 
  mutate(
    dags = clock::date_build(ar, manudur)
  ) |> 
  pivot_longer(c(alvarleg, samtals)) |> 
  mutate(
    value = slider::slide_dbl(value, mean, .before = 12),
    .by = name
  ) |> 
  inner_join(
    pop,
    by = "ar",
  ) |> 
  mutate(
    pop = lm(pop ~ row_number()) |> predict(),
    value = value / pop * 1e5,
    .by = name
  ) 


p1 <- plot_dat |> 
  filter(name == "alvarleg") |> 
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
    breaks = breaks_pretty(),
    labels = label_number(),
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Alvarlegar líkamsárasir á Höfuðborgarsvæðinu"
  )


p2 <- plot_dat |> 
  filter(name == "samtals") |> 
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
    breaks = breaks_pretty(),
    labels = label_number(),
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Ofbeldisbrot samtals á Höfuðborgarsvæðinu"
  )

p3 <- plot_dat |> 
  pivot_wider() |> 
  mutate(
    p = alvarleg / samtals
  ) |> 
  ggplot(aes(dags, p)) +
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
    breaks = breaks_pretty(),
    labels = label_hlutf(accuracy = 1),
    limits = c(0, 0.25),
    expand = expansion(),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Hlutfall alvarlegra líkamsárása af öllum ofbeldisbrotum á Höfuðborgarsvæðinu"
  )



p <- (p1 + p2) / 
  p3 +
  plot_annotation(
    title = "Ofbeldisbrot á Höfuðborgarsvæðinu undanfarinn áratug",
    subtitle = "Tölur sýndar sem meðaltöl ársins sem leið hverri stundu. Fjöldatölur sýndar á 100.000 íbúa Höfuðborgarsvæðis."
  )

p

ggsave(
  plot = p,
  filename = "Figures/ofbeldi.png",
  width = 8, height = 0.6 * 8, scale = 1.3
)
