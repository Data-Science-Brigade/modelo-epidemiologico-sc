# FOLLOW UP

library(epiCata)

# Load latest model
load("results/2020_07_20/2020_07_20_base_FULL_164994-stanfit.Rdata")

reference_date_str <- model_output$reference_date_str
reference_date = ymd(reference_date_str)

dfs <- get_forecast_dfs(location_name, model_output)
cum_deaths <- get_cumulative_df(data_location=dfs$data_location,
                                dfs$data_location_forecast,
                                reference_date = ymd(reference_date_str))

followup <- cum_deaths %>%
  filter(time <= ymd(reference_date)- days(1)) %>%
  select(time, `Cenario 1`) %>% rename(`Obitos confirmados`=`Cenario 1`) %>%
  gather("key" = key, "value" = value, -time) %>% drop_na()

cumulative_deaths <- cum_deaths %>%
  gather("key" = key, "value" = value, -time) %>%
  filter(time >= ymd(reference_date) - days(1)) %>% drop_na()

cumulative_deaths <- bind_rows(cumulative_deaths, followup)
fill_labels <- c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados")

cumulative_deaths$key <-
  factor(cumulative_deaths$key,
         levels=c("Cenario 1", "Cenario 2", "Cenario 3", "Obitos confirmados"),
         labels=fill_labels)

color_vals <- c("#FFA600", "#FF6F00", "#FF0000", "#7300FF")
names(color_vals) <- levels(cumulative_deaths$key)

x_breaks_by <- "weeks"
x_min <- ymd("2020-05-31")
x_max <- ymd("2020-08-16")

x_breaks <- seq(x_min, x_max,by=x_breaks_by)


cumulative_deaths <- cumulative_deaths %>% filter(time >= x_min)

p <- ggplot(mapping=aes(x=time, y=value, label=value, color=key, group=key)) +
  geom_line(data=cumulative_deaths %>% filter(key != "Obitos confirmados"),
            size=0.5, alpha=0.5, linetype="dashed") +
  geom_line(data=cumulative_deaths %>% filter(key == "Obitos confirmados"),
            size=1, linetype="solid", color="#7300FF") +
  geom_point(data=cumulative_deaths %>% filter(time %in% x_breaks), alpha=1, size=2) +
  geom_label_repel(data=cumulative_deaths %>% filter(time == x_max),
                   size=4, show.legend = FALSE) +
  geom_label(data=cumulative_deaths %>% filter(time == ymd(reference_date) - days(1)),
             size=4, show.legend = FALSE, fill="#7300FF", color="#FFFFFF")

# Workaround to try to get better looking Y axis legends
min_y_break <- min(cumulative_deaths$value)
n_integer_digits <- floor(log10(min_y_break)) + 1
min_y_break <- floor(min_y_break/10^(n_integer_digits - 1)) * 10^(n_integer_digits - 1)

max_y_break <- max(cumulative_deaths$value)
n_integer_digits <- floor(log10(max_y_break)) + 1
max_y_break <- ceiling(max_y_break/10^(n_integer_digits - 1)) * 10^(n_integer_digits - 1)

y_separation <- floor(max_y_break - min_y_break)
y_breaks <- seq(min_y_break, max_y_break + (y_separation %% 4), floor(y_separation/4))

# Modify y_breaks manually if the above didn't produce any nicer results
y_breaks <- seq(0, 2400, 400)

pp <- p +
  geom_label(data=cumulative_deaths %>% filter(key == "Cenário Real", time >= ymd(reference_date)),
             label.padding = unit(0.2, "lines"), size=4) +
  xlab("Data") +
  ylab("Número TOTAL de óbitos\n") +
  scale_x_date(labels = date_format("%e %b"),
               breaks=x_breaks,
               limits=c(x_min - days(1), x_max)) +
  scale_y_continuous(limits=c(min(y_breaks), max(y_breaks)), breaks=y_breaks) +
  theme_dsb_light() +
  theme(panel.grid.major.x = element_line(linetype = "dotted", color = "grey", size=0.4),
        legend.text = element_text(margin = margin(l=2.5, t=0)),
        legend.title = element_text(margin=margin(t=0))) +
  scale_color_manual(name="Cenarios", values=color_vals) +
  scale_fill_manual(name="Cenarios", values=color_vals)

plot_filename <- sprintf("figures/%s/FOLLOWUP_%s_%s.png",
                         reference_date_str,
                         location_name,
                         model_output$filename_suffix, ".png")
cat(sprintf("\n   Saving %s", plot_filename))
ggsave(file= plot_filename, p, width = 11.08, height=4.8, type="cairo")
