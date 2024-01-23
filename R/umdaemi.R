library(tidyverse)
library(metill)
library(readxl)
library(hagstofa)
library(ggh4x)
library(patchwork)
library(geomtextpath)
theme_set(theme_metill())

caption <- str_c(
  "Mynd eftir @bggjonsson hjá metill.is byggð á árlegum tölum Ríkislögreglustjóra",
  "\nGögn og kóði: https://github.com/bgautijonsson/logregla"
)

d <- read_excel(
  "Data/1.FJOLDI-BROTA-og-BROT-A-IBUA_stadfestar-tolur.xlsx",
  sheet = 2
  )


p <- d |> 
  pivot_longer(-c(1:4), names_to = "ar", values_to = "fjoldi") |> 
  mutate(
    ar = parse_number(ar)
  ) |> 
  janitor::clean_names() |> 
  filter(
    yfirflokkur == "Öll brot",
    umdaemi != "Öll embætti"
  ) |> 
  ggplot(aes(ar, fjoldi)) +
  geom_line(
    linewidth = 1.5
  ) +
  geom_line(
    data = ~ rename(.x, umd = umdaemi),
    aes(group = umd),
    alpha = 0.2,
    linewidth = 0.3
  ) +
  scale_x_continuous(
    breaks = seq(2001, 20022, by = 3),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = breaks_pretty(),
    limits = c(0, NA),
    expand = expansion(),
    guide = guide_axis_truncated(
      trunc_upper = 5000
    )
  ) +
  coord_cartesian(
    ylim = c(0, 5100)
  ) +
  facet_wrap("umdaemi") +
  theme(
    panel.spacing.x = unit(0.4, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Árlegur fjöldi afbrota eftir umdæmi",
    subtitle = "Tölur sýndar sem fjöldi afbrota á 100.000 íbúa",
    caption = caption
  )


p



ggsave(
  plot = p,
  filename = "Figures/umdaemi.png",
  width = 8, height = 0.621 * 8, scale = 1.5
)









plot_dat <- d |> 
  pivot_longer(-c(1:4), names_to = "ar", values_to = "fjoldi") |> 
  mutate(
    ar = parse_number(ar)
  ) |> 
  janitor::clean_names() |> 
  filter(
    yfirflokkur == "Öll brot",
    umdaemi != "Öll embætti"
  ) |> 
  arrange(ar, fjoldi) |> 
  mutate(
    saeti = row_number(),
    .by = ar
  ) 


p <- plot_dat |> 
  bind_rows(
    plot_dat |> 
      filter(ar == 2022) |> 
      mutate(ar = 2023)
  ) |> 
  ggplot(aes(ar, saeti)) +
  geom_step(
    linewidth = 1.5
  ) +
  geom_hline(
    yintercept = 1:9,
    alpha = 0.1,
    linewidth = 0.2
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2022, by = 3) + 0.5,
    labels = \(x) floor(x),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    breaks = 1:9,
    limits = c(0, 10),
    labels = \(x) 10 - x,
    expand = expansion(),
    guide = guide_axis_truncated()
  ) +
  facet_wrap("umdaemi") +
  theme(
    panel.spacing.x = unit(0.4, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Lögregluumdæmum raðað í sæti eftir afbrotatíðni",
    subtitle = "1: Hæsta afbrotatíðnin | 9: Lægsta afbrotatíðnin",
    caption = caption
  )

p

ggsave(
  plot = p,
  filename = "Figures/umdaemi_ordered.png",
  width = 8, height = 0.621 * 8, scale = 1.5
)

