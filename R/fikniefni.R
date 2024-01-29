library(tidyverse)
library(metill)
library(readxl)
library(hagstofa)
library(ggh4x)
library(patchwork)
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

d <- read_excel("data/logregla_hofudborgarsvaedis.xlsx")


plot_dat <- d |> 
  janitor::clean_names() |> 
  fill(ar) |>
  arrange(ar, man) |> 
  mutate(
    dags = clock::date_build(ar, man)
  ) |> 
  select(dags, ar, man, alvarleg = fikniefni_storfelld, samtals = fikniefni) |> 
  pivot_longer(c(alvarleg, samtals)) |> 
  mutate(
    value = slider::slide_dbl(value, mean, .before = 11),
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
    subtitle = "Stórfelld fíkniefnabrot á Höfuðborgarsvæðinu"
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
    subtitle = "Fíkniefnabrot samtals á Höfuðborgarsvæðinu"
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
    limits = c(0, 0.1),
    expand = expansion(),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Hlutfall stórfelldra brota af öllum fíkniefnabrotum á Höfuðborgarsvæðinu"
  )



p <- (p1 + p2) / 
  p3 +
  plot_annotation(
    title = "Fíkniefnabrot á Höfuðborgarsvæðinu undanfarinn áratug",
    subtitle = str_c(
      "Tölur sýndar sem meðaltöl undanfarins árs | ",
      "Fjöldatölur sýndar á 100.000 íbúa Höfuðborgarsvæðis"
      ),
    caption = caption
  )

p

ggsave(
  plot = p,
  filename = "Figures/fikniefni.png",
  width = 8, height = 0.6 * 8, scale = 1.3
)

