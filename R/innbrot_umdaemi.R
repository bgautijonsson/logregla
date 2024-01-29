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


span <- 0.5
read_excel(
  "Data/1.FJOLDI-BROTA-og-BROT-A-IBUA_stadfestar-tolur.xlsx",
  sheet = 2
) |> 
  pivot_longer(-c(1:4), names_to = "ar", values_to = "fjoldi") |> 
  mutate(
    ar = parse_number(ar)
  ) |> 
  janitor::clean_names() |> 
  filter(
    str_detect(
      tegund_brots,
      "Líkamsárás \\(217"
    ),
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
    )
  ) +
  coord_cartesian(
    ylim = c(0, 75)
  ) +
  facet_wrap("umdaemi") +
  theme(
    panel.spacing.x = unit(0.4, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Árlegur fjöldi innbrota eftir umdæmi",
    subtitle = "Tölur sýndar sem fjöldi afbrota á 100.000 íbúa",
    caption = caption
  )


p1 <- read_excel(
  "Data/1.FJOLDI-BROTA-og-BROT-A-IBUA_stadfestar-tolur.xlsx",
  sheet = 2
) |> 
  pivot_longer(-c(1:4), names_to = "ar", values_to = "fjoldi") |> 
  mutate(
    ar = parse_number(ar)
  ) |> 
  janitor::clean_names() |> 
  filter(
    tegund_brots == "Innbrot (244. gr.)",
    umdaemi != "Öll embætti"
  ) |> 
  ggplot(aes(ar, fjoldi)) +
  stat_smooth(
    geom = "line",
    linewidth = 1.5,
    span = span,
    arrow = arrow(length = unit(0.15, "inches"), type = "closed")
  ) +
  stat_smooth(
    geom = "line",
    span = span,
    data = ~ rename(.x, umd = umdaemi),
    aes(group = umd),
    alpha = 0.1,
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
    )
  ) +
  coord_cartesian(
    ylim = c(0, 150)
  ) +
  facet_wrap("umdaemi") +
  theme(
    panel.spacing.x = unit(0.4, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Árlegur fjöldi innbrota eftir umdæmi",
    subtitle = "Tölur sýndar sem fjöldi afbrota á 100.000 íbúa",
    caption = caption
  )

p1

ggsave(
  plot = p1,
  filename = "Figures/innbrot_umdaemi_perpop.png",
  width = 8, height = 0.621 * 8, scale = 1.5
)


p2 <- read_excel(
  "Data/1.FJOLDI-BROTA-og-BROT-A-IBUA_stadfestar-tolur.xlsx",
  sheet = 1
) |> 
  pivot_longer(-c(1:4), names_to = "ar", values_to = "fjoldi") |> 
  mutate(
    ar = parse_number(ar)
  ) |> 
  janitor::clean_names() |> 
  filter(
    tegund_brots == "Innbrot (244. gr.)",
    !umdaemi %in% c("Öll embætti/All districts", "Ríkissaksóknari", "Sérstakur saksóknari/Héraðssaksóknari*", "Ríkislögreglustjórinn")
  ) |> 
  ggplot(aes(ar, fjoldi)) +
  stat_smooth(
    geom = "line",
    span = span,
    linewidth = 1.5,
    arrow = arrow(length = unit(0.15, "inches"), type = "closed")
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
    )
  ) +
  coord_cartesian(
    # ylim = c(0, 150)
  ) +
  facet_wrap("umdaemi", scales = "free_y") +
  theme(
    panel.spacing.x = unit(0.4, "cm")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Árlegur fjöldi innbrota eftir umdæmi",
    subtitle = "Tölur sýndar sem hreinn fjöldi afbrota",
    caption = caption
  )

p2

ggsave(
  plot = p2,
  filename = "Figures/innbrot_umdaemi_rawcount.png",
  width = 8, height = 0.621 * 8, scale = 1.5
)

