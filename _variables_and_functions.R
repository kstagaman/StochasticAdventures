source("C:/Users/kstag/OneDrive/Documents/GitHub/StochasticAdventures/_packages_and_environments.R")

### Basic variables ###
raw.roll <- NULL
raw.roll$nor <- 1:20
raw.roll$adv <- sapply(1:20, function(a) { sapply(1:20, function(b) { max(c(a, b)) }) })
raw.roll$dis <- sapply(1:20, function(a) { sapply(1:20, function(b) { min(c(a, b)) }) })
test.roll <- NULL
test.roll$nor <- data.table(Roll = raw.roll$nor, Hit.wgt = 1 / 20)
test.roll$adv <- as.vector(raw.roll$adv) %>%
  table() %>%
  as.data.table() %>%
  set_names(c("Roll", "Hit.wgt"))
test.roll$adv[, `:=`(Roll = as.integer(Roll), Hit.wgt = Hit.wgt / sum(Hit.wgt))]
test.roll$dis <- as.vector(raw.roll$dis) %>%
  table() %>%
  as.data.table() %>%
  set_names(c("Roll", "Hit.wgt"))
test.roll$dis[, `:=`(Roll = as.integer(Roll), Hit.wgt = Hit.wgt / sum(Hit.wgt))]

ability.gen.methods <- list(
  gen1 = data.table(
    Method = "Standard Array",
    Level = 1:20,
    A1 = c(17, 17, 17, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20),
    A2 = c(15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 18, 18, 18, 18, 18),
    A3 = c(13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14),
    A4 = c(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
    A5 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
    A6 = c( 8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8)
  ),
  gen2 = data.table(
    Method = "Point Buy",
    Level = 1:20,
    A1 = c(17, 17, 17, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20),
    A2 = c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 18, 18, 18, 18, 20, 20, 20, 20, 20),
    A3 = c(13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14),
    A4 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
    A5 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10),
    A6 = c( 8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8)
  )
)
abilities <- rbindlist(ability.gen.methods)
abilities[, Prof.bonus := rep(as.vector(sapply(2:6, rep, 4)), length(ability.gen.methods))]
abilities %<>% cbind(
  lapply(abilities[, paste0("A", 1:6), with = F], function(x) {
    floor({x - 10} / 2)
  }) %>%
    as.data.table() %>%
    set_names(paste0("A", 1:6, ".mod"))
)


### Aesthetic Variables ###
rollType.abrevs <- c(adv = "advantage", nor = "normal", dis = "disadvantage")
rollType.lvls <- unname(rollType.abrevs)
rollType.colors <- c(brewer.pal("YlOrRd", n = 9)[4], "black", brewer.pal("Blues", n = 9)[8])
names(rollType.colors) = rollType.lvls

hitType.abrevs <- c(suc = "crit. hit", reg = "regular", fai = "crit. miss")
hitType.lvls <- unname(hitType.abrevs)
hitType.colors <- brewer.pal("Set1", n = 9)[c(3, 9, 1)]
names(hitType.colors) = hitType.lvls

### Functions ###
rep.dmg <- function(dt, rep.num = 1) {
  dt1 <- copy(dt)
  for (i in 1:rep.num) {
    dt1 <- lapply(1:nrow(dt), function(r) {
      dt2 <- copy(dt1)
      dt2[, `:=`(Dmg = Dmg + dt[r]$Dmg, Dmg.wgt = Dmg.wgt * dt[r]$Dmg.wgt)]
      return(dt2)
    }) %>%
      rbindlist() %>%
      .[, .(Dmg.wgt = sum(Dmg.wgt)), by = c(names(dt)[-str_which(names(dt), "Dmg.wgt")])]
  }
  return(dt1)
}
add.dmg <- function(dt1, dt2) {
  dt3 <- lapply(1:nrow(dt1), function(r) {
    dt2.1 <- copy(dt2)
    dt2.1[, `:=`(Dmg = Dmg + dt1[r]$Dmg, Dmg.wgt = Dmg.wgt * dt1[r]$Dmg.wgt)]
    return(dt2.1)
  }) %>%
    rbindlist() %>%
    .[, .(Dmg.wgt = sum(Dmg.wgt)), by = c(names(dt1)[-str_which(names(dt1), "Dmg.wgt")])]

  return(dt3)
}
dmg.dist <- function(Ns = c(1), Ss = c(6)) {
  if (length(Ns) == 1 & length(Ss) > 1) {
    Ns %<>% rep(length(Ss))
  } else if (length(Ns) > 1 & length(Ss) == 1) {
    Ss %<>% rep(length(Ns))
  } else if (length(Ns) > 1 & length(Ss) > 1 & length(Ns) != length(Ss)) {
    rlang::abort("if Ns and Ss are of lengths greater than 1, they must be equal")
  }

  dt1 <- data.table(Dmg = 1:Ss[1], Dmg.wgt = 1/Ss[1])
  if (Ns[1] > 1) {
    dt1 %<>% rep.dmg(rep.num = {Ns[1] - 1})
  }
  if (length(Ns) > 1) {
    for (i in seq_along(Ns)[-1]) {
      dt1.2 <- data.table(Dmg = 1:Ss[i], Dmg.wgt = 1/Ss[i])
      if (Ns[i] > 1) {
        dt1.2 %<>% rep.dmg(rep.num = {Ns[i] - 1})
      }
      dt1 %<>% add.dmg(dt2 = dt1.2)
    }
  }
  dt2 <- rep.dmg(dt = dt1, rep.num = 1)
  list(Strt = dt1, Crit = dt2) %>% return()
}

parse.dmg.dice <- function(dmg.str) {
  r.names <- c("N", "S")
  if (str_detect(dmg.str,"\\+")) {
    dmg.strs <- str_split(dmg.str, " *\\+ *")[[1]]
    res <- sapply(dmg.strs, parse.dmg.dice) %>% set_rownames(r.names)
  } else if (str_detect(dmg.str, "^\\d+d\\d+$")) {
    res <- str_split(dmg.str, "d")[[1]] %>%
      as.numeric() %>%
      matrix(ncol = 1, dimnames = list(r.names, dmg.str))
  } else if (str_detect(dmg.str, "^d\\d+$")) {
    res <- str_remove(dmg.str, "^d") %>%
      as.numeric() %>%
      c(1, .) %>%
      matrix(ncol = 1, dimnames = list(r.names, dmg.str))
  }
  return(res)
}

make.attack <- function(
    roll.type = c("normal", "advantage", "disadvantage"),
    AC = 14,
    atk.bns = 5,
    dmg.dice = "1d6",
    dmg.bns = 3,
    by.hitType = FALSE
) {
  roll.type <- rlang::arg_match(roll.type, c("normal", "advantage", "disadvantage"))
  rt <- strtrim(roll.type, 3)
  Dmg.info <- parse.dmg.dice(dmg.dice)
  dmg.list <- dmg.dist(Ns = Dmg.info["N", ], Ss = Dmg.info["S", ])
  dmg.dt <- dmg.list$Strt
  crit.dt <- dmg.list$Crit

  dmg.dt[
    , Wgt := sum(test.roll[[rt]][Roll + atk.bns >= AC & !{Roll %in% c(1, 20)}]$Hit.wgt) * Dmg.wgt
  ]
  crit.dt[, Wgt := sum(test.roll[[rt]][Roll == 20]$Hit.wgt) * Dmg.wgt]

  if (by.hitType) {
    all.dmg.dt <- rbind(
      data.table(
        Dmg = 0,
        Dmg.wgt = NA,
        Wgt = sum(test.roll[[rt]][Roll > 1 & Roll + atk.bns < AC]$Hit.wgt),
        Hit.type = "regular"
      ),
      data.table(
        Dmg = 0,
        Dmg.wgt = NA,
        Wgt = sum(test.roll[[rt]][Roll == 1]$Hit.wgt),
        Hit.type = "crit. miss"
      ),
      dmg.dt[, `:=`(Dmg = {Dmg + dmg.bns}, Hit.type = "regular")],
      crit.dt[, `:=`(Dmg = {Dmg + dmg.bns}, Hit.type = "crit. hit")]
    ) %>%
      .[, .(Wgt = sum(Wgt)), by = c("Hit.type", "Dmg")]
  } else {
    all.dmg.dt <- rbind(
      data.table(
        Dmg = 0,
        Dmg.wgt = NA,
        Wgt = sum(test.roll[[rt]][Roll + atk.bns < AC | Roll == 1]$Hit.wgt)
      ),
      dmg.dt[, Dmg := Dmg + dmg.bns],
      crit.dt[, Dmg := Dmg + dmg.bns]
    ) %>%
      .[, .(Wgt = sum(Wgt)), by = "Dmg"]
  }
  return(all.dmg.dt)
}

multiply.action <- function(atk.res, n.atks = 2) {
  if (n.atks == 1) {
    return(atk.res)
  } else {
    curr.dt <- copy(atk.res)
    for (a in 1:(n.atks - 1)) {
      curr.dt <- lapply(1:nrow(curr.dt), function(r) {
        new.dt <- copy(atk.res)
        new.dt[, `:=`(Dmg = Dmg + curr.dt[r]$Dmg, Wgt = Wgt * curr.dt[r]$Wgt)]
        return(new.dt)
      }) %>%
        rbindlist() %>%
        .[, .(Wgt = sum(Wgt)), by = c(names(atk.res)[-str_which(names(atk.res), "Wgt")])]
    }
    return(curr.dt)
  }
}

cause.save <- function(
    roll.type = c("normal", "advantage", "disadvantage"),
    DC = 14,
    save.bns = 3,
    dmg.dice = "1d6",
    dmg.on.save = c("half", "none"),
    dmg.bns = 0
) {
  roll.type <- rlang::arg_match(roll.type, c("normal", "advantage", "disadvantage"))
  dmg.on.save <- rlang::arg_match(dmg.on.save, c("half", "none"))
  rt <- strtrim(roll.type, 3)
  Dmg.info <- parse.dmg.dice(dmg.dice)
  dmg.list <- dmg.dist(Ns = Dmg.info["N", ], Ss = Dmg.info["S", ])
  dmg.dt <- dmg.list$Strt
  dmg.mult <- c(half = 0.5, none = 0)

  noSave.dt <- copy(dmg.dt)
  save.dt <- copy(dmg.dt)
  noSave.dt[, Wgt := sum(test.roll[[rt]][Roll + save.bns < DC ]$Hit.wgt) * Dmg.wgt]
  save.dt[, Wgt := sum(test.roll[[rt]][Roll + save.bns >= DC ]$Hit.wgt) * Dmg.wgt]

  all.dmg.dt <- rbind(
    save.dt[, Dmg := floor({ Dmg + dmg.bns } * dmg.mult[dmg.on.save])],
    noSave.dt[, Dmg := Dmg + dmg.bns]
  ) %>%
    .[, .(Wgt = sum(Wgt)), by = "Dmg"]
  return(all.dmg.dt)
}

action.sim <- function(
    name,
    type = c("attack", "save"),
    ac.range = 8:18,
    save.bns.range = 0:10,
    ...
) {
  type <- rlang::arg_match(type, c("attack", "save"))
  action.func <- list(attack = "make.attack", save = "cause.save")
  var.nms <- list(attack = "AC", save = "save.bns")
  all.args <- rlang::list2(...)
  nm.func <- do.call("formals", list(action.func[[type]])) %>% names()
  nm.multiply <- names(formals(multiply.action))
  if (!{"n.atks" %in% names(all.args)}) { all.args[["n.atks"]] <- 1 }
  target.grid <- expand.grid(ac.range, save.bns.range) %>%
    as.data.table() %>%
    set_names(c("attack", "save"))
  lapply(1:nrow(target.grid), function(r) {
    var.val <- list(target.grid[r][[type]]) %>% set_names(var.nms[[type]])
    atk.init <- do.call(action.func[[type]], c(var.val, all.args[names(all.args) %in% nm.func]))
    atk.all <- do.call(
      "multiply.action",
      c(list(atk.res = atk.init), all.args[names(all.args) %in% nm.multiply])
    )
    atk.all[, `:=`(Target.AC = target.grid[r]$attack, Target.save = target.grid[r]$save)]
    return(atk.all)
  }) %>%
    rbindlist() %>%
    .[, `:=`(Action = name, Type = type)] %>%
    return()
}

create.fig.dir <- function(x) {
  dir.name <- file.path(dirs$figs, paste0("Post", str_pad(x, width = 4, pad = 0)))
  if (!dir.exists(dir.name)) { dir.create(dir.name) }
  return(dir.name)
}

primary.attack.bonus <- function(level, ability.gen.method = c("Standard Array", "Point Buy")) {
  ability.gen.method <- arg_match(ability.gen.method, values = c("Standard Array", "Point Buy"))
  if (!is_integerish(level, n = 1) || level < 1 || level > 20) {
    abort(
      message = c(
        "x" = paste("`level` must be a single integer between 1 and 20, inclusive."),
        "i" = paste0("You supplied a value of type <", typeof(level), "> with value: ", level)
      ),
      class = "error_invalid_range"
    )
  }
  abilities[Level == level & Method == ability.gen.method, .(Res = A1.mod + Prof.bonus)]$Res %>%
    return()
}

secondary.attack.bonus <- function(level, ability.gen.method = c("Standard Array", "Point Buy")) {
  ability.gen.method <- arg_match(ability.gen.method, values = c("Standard Array", "Point Buy"))
  if (!is_integerish(level, n = 1) || level < 1 || level > 20) {
    abort(
      message = c(
        "x" = paste("`level` must be a single integer between 1 and 20, inclusive."),
        "i" = paste0("You supplied a value of type <", typeof(level), "> with value: ", level)
      ),
      class = "error_invalid_range"
    )
  }
  abilities[Level == level & Method == ability.gen.method, .(Res = A2.mod + Prof.bonus)]$Res %>%
    return()
}

primary.attack.mod <- function(level, ability.gen.method = c("Standard Array", "Point Buy")) {
  ability.gen.method <- arg_match(ability.gen.method, values = c("Standard Array", "Point Buy"))
  if (!is_integerish(level, n = 1) || level < 1 || level > 20) {
    abort(
      message = c(
        "x" = paste("`level` must be a single integer between 1 and 20, inclusive."),
        "i" = paste0("You supplied a value of type <", typeof(level), "> with value: ", level)
      ),
      class = "error_invalid_range"
    )
  }
  abilities[Level == level & Method == ability.gen.method]$A1.mod %>%
    return()
}

secondary.attack.mod <- function(level, ability.gen.method = c("Standard Array", "Point Buy")) {
  ability.gen.method <- arg_match(ability.gen.method, values = c("Standard Array", "Point Buy"))
  if (!is_integerish(level, n = 1) || level < 1 || level > 20) {
    abort(
      message = c(
        "x" = paste("`level` must be a single integer between 1 and 20, inclusive."),
        "i" = paste0("You supplied a value of type <", typeof(level), "> with value: ", level)
      ),
      class = "error_invalid_range"
    )
  }
  abilities[Level == level & Method == ability.gen.method]$A2.mod %>%
    return()
}
