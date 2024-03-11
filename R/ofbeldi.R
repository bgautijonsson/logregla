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

d <- read_excel("data/logregla_hofudborgarsvaedis.xlsx")


plot_dat <- d |> 
  janitor::clean_names() |> 
  fill(ar) |>
  arrange(ar, man) |> 
  mutate(
    dags = clock::date_build(ar, man)
  ) |> 
  select(
    dags, ar, man, 
    alvarleg = ofbeldi_alvarlegt, 
    minnihattar = ofbeldi_minnihattar,
    samtals = ofbeldi_samtals,
    heimilis = heimilisofbeldi
  ) |> 
  pivot_longer(c(alvarleg, samtals, heimilis, minnihattar)) |> 
  mutate(
    value = slider::slide_dbl(value, mean, .before = 11, na.rm = TRUE),
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
    limits = c(0, 50),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  theme(
    axis.text.x = element_text(size = 7)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Alvarlegar líkamsárasir"
  )

p2 <- plot_dat |> 
  filter(name == "heimilis") |> 
  ggplot(aes(dags, value)) +
  annotate(
    geom = "text",
    label = "Ekki skráð",
    x = clock::date_build(2015, 4, 1),
    y = 18.7,
    angle = 72,
    size = 3.5
  ) +
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
    limits = c(0, 50),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  theme(
    axis.text.x = element_text(size = 7)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Tilkynnt heimilisofbeldi"
  )

p3 <- plot_dat |> 
  select(-pop) |> 
  pivot_wider() |> 
  mutate(
    value = samtals - heimilis
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
    breaks = breaks_pretty(),
    labels = label_number(),
    limits = c(0, 50),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  theme(
    axis.text.x = element_text(size = 7)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Annað en heimilisofbeldi"
  )

p4 <- plot_dat |> 
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
    limits = c(0, 50),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  theme(
    axis.text.x = element_text(size = 7)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Ofbeldisbrot samtals"
  )



p5 <- plot_dat |> 
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
    breaks = breaks_pretty(7),
    labels = label_hlutf(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(),
    guide = guide_axis_truncated(
      trunc_upper = 1
    )
  ) +
  theme(
    axis.text.x = element_text(size = 9)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Alvarlegar líkamsárásir (% af heild)"
  )

p6 <- plot_dat |> 
  pivot_wider() |> 
  mutate(
    p = heimilis / samtals
  ) |> 
  ggplot(aes(dags, p)) +
  annotate(
    geom = "text",
    label = "Ekki skráð",
    x = clock::date_build(2015, 4, 30),
    y = 0.455,
    angle = 65,
    size = 3.5
  ) +
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
    breaks = breaks_pretty(7),
    labels = label_hlutf(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(),
    guide = guide_axis_truncated(
      trunc_upper = 1
    )
  ) +
  theme(
    axis.text.x = element_text(size = 9)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Tilkynnt heimilisofbeldi (% af heild)"
  )

p7 <-  plot_dat |> 
  select(-pop) |> 
  pivot_wider() |> 
  mutate(
    value = samtals - heimilis,
    p = value / samtals
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
    breaks = breaks_pretty(7),
    labels = label_hlutf(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(),
    guide = guide_axis_truncated(
      trunc_upper = 1
    )
  ) +
  theme(
    axis.text.x = element_text(size = 9)
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Annað en heimilisofbeldi (% af heild)"
  )




p <- (p1 + p2 + p3 + p4 + plot_layout(nrow = 1)) / 
  (p5 + p6 + p7 + plot_layout(nrow = 1)) +
  plot_annotation(
    title = "Ofbeldisbrot á Höfuðborgarsvæðinu undanfarinn áratug",
    subtitle = str_c(
      "Tölur sýndar sem meðaltöl undanfarins árs | ",
      "Fjöldatölur sýndar á 100.000 íbúa Höfuðborgarsvæðis"
    ),
    caption = caption
  )

p


ggsave(
  plot = p,
  filename = "Figures/ofbeldi.png",
  width = 8, height = 0.621 * 8, scale = 1.7
)

ggsave(
  plot = p &
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ),
  filename = "Figures/ofbeldi_fp.png",
  width = 8, height = 0.621 * 8, scale = 1.3 
)

