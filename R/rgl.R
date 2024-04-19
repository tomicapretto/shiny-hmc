# This file contains helper functions to work with rgl.
# I wrote them by taking pieces out of existing functions in the rgl package. 
# These custom functions are faster than the functions in rgl because they are
# specific for the type of objects we use in the application.

DEFAULT_MATERIAL <- rgl::material3d()

material_diff <- function(material) {
  for (name in names(material)) {
    if (identical(material[[name]], DEFAULT_MATERIAL[[name]]))
      material[[name]] <- NULL
  }
  material
}

getObject <- function(id, type) {
  # Make result container
  result <- list(id = id, type = type)
  
  # Get "lit" and "material"
  material <- rgl:::rgl.getmaterial(id = id)
  lit <- material$lit
  result$material <- material_diff(material)
  
  attribs <- c(
    "vertices", "colors", "texcoords", "dim", "texts", "cex", "adj", "radii", 
    "ids", "usermatrix", "types", "offsets", "centers", "family", "font", "pos",
    "axes", "indices", "normals"
  )
  
  # Store object's attributes
  for (attrib in attribs) {
    if (rgl::rgl.attrib.count(id, attrib)) {
      result[[attrib]] <- rgl::rgl.attrib(id, attrib)
    }
  }
  
  # Get object's flag
  flags <- rgl::rgl.attrib(id, "flags")
  
  # NOTE: The only flag we're using so far is "ignoreExtent"
  if (length(flags)) {
    if ("ignoreExtent" %in% rownames(flags)) 
      result$ignoreExtent <- flags["ignoreExtent", 1]
    if ("fixedSize" %in% rownames(flags))
      result$fixedSize <- flags["fixedSize", 1]
    if ("rotating" %in% rownames(flags)) 
      result$rotating <- flags["rotating", 1]
    if ("fastTransparency" %in% rownames(flags)) 
      result$fastTransparency <- flags["fastTransparency", 1]
    if ("flipped" %in% rownames(flags)) 
      result$flipped <- flags["flipped", 1]
  }
  
  # Make result be of a specific class
  class(result) <- c(paste0("rgl", type), "rglobject")
  result
}

makeList <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, makeList)
  }
  if (length(names(x))) {
    x <- as.list(x)
  }
  x
}

flagnames <- c(
  "is_lit", "is_smooth", "has_texture", "depth_sort", "fixed_quads", 
  "is_transparent", "is_lines", "sprites_3d", "is_subscene", "is_clipplanes",
  "fixed_size", "is_points", "is_twosided", "fat_lines", "is_brush", "has_fog",
  "rotating"
)

getMaterial <- function(obj) {
  material <- obj$material
  missing <- setdiff(names(DEFAULT_MATERIAL), names(material))
  material[missing] <- DEFAULT_MATERIAL[missing]
  material
}

getFlags <- function(obj) {
  type <- obj$type
  
  result <- structure(rep(FALSE, length(flagnames)), names = flagnames)
  
  result["is_transparent"] <- any(obj$colors[, "a"] < 1); # More later...
  
  mat <- getMaterial(obj)
  
  lit_types <- c(
    "triangles", "quads", "surface", "planes", "spheres", "sprites", "bboxdeco"
  )
  result["is_lit"] <- mat$lit && type %in% lit_types
  
  smooth_types <- c("triangles", "quads", "surface", "planes", "spheres")
  result["is_smooth"] <- mat$smooth && type %in% smooth_types
  
  
  result["sprites_3d"] <- sprites_3d <- type == "sprites" && length(obj$ids)
  
  result["has_texture"] <- has_texture <- !is.null(mat$texture) &&
    (
      !is.null(obj$texcoords) ||
      (type == "sprites" && !sprites_3d) ||
      (type == "background" && obj$sphere) ||
      (type == "spheres")
    )
  
  result["is_transparent"] <- is_transparent <- (
    (has_texture && mat$isTransparent) || result["is_transparent"]
  )
  
  result["depth_sort"] <- depth_sort <- (
    is_transparent && type %in% c(
      "triangles", "quads", "surface","spheres", "sprites", "text"
    )
  )
  
  result["fixed_quads"] <- type %in% c("text", "sprites") && !sprites_3d
  result["is_lines"]    <- type %in% c("lines", "linestrip", "abclines")
  result["is_points"]   <- type == "points" || "points" %in% c(mat$front, mat$back)
  result["is_twosided"] <- (
      type %in% c("quads", "surface", "triangles", "spheres", "bboxdeco") && 
      length(unique(c(mat$front, mat$back))) > 1
  ) || (type == "background" && obj$sphere)
  
  if (result["is_twosided"] && !is.null(obj$indices) && is.null(obj$normals)) {
    warning("Object ", obj$id, " is two-sided and indexed.  It requires normals.")
    result["is_twosided"] <- FALSE
  }
  
  result["fixed_size"]  <- type == "text" || isTRUE(obj$fixedSize)
  result["rotating"] <- isTRUE(obj$rotating)
  result["fat_lines"] <- (
    mat$lwd != 1 && 
      (result["is_lines"] || "lines" %in% unlist(mat[c("front", "back")]))
  )
  result["is_brush"] <- FALSE
  result["has_fog"] <- mat$fog
  result
}

# Encode flags into a numeric value
numericFlags <- function(flags) {
  if (is.matrix(flags)) {
    n <- ncol(flags)
  } else {
    n <- length(flags)
  }
  unname(flags %*% 2 ^ (seq_len(n) - 1))
}

get_objects <- function(objects, root) {
  # Query the IDs of the shapes in the currently active subscene.
  objects_ids <- rgl::ids3d("shapes")
  
  # Keep only the information about the objects we're interested in
  objects_ids <- objects_ids[objects_ids$id %in% objects, ]
  
  # Create output container 
  objects_list <- vector("list", nrow(objects_ids))
  
  ids <- objects_ids$id
  ids_char <- as.character(objects_ids$id)
  types <- as.character(objects_ids$type)
  
  # Get objects into the output list
  for (i in seq_along(objects_list)) {
    objects_list[[i]] <- getObject(ids[i], types[i])
    names(objects_list)[i] <- ids_char[i]
  }
  
  # Listify the attributes of the objects
  objects_list <- makeList(objects_list)
  
  # Add two relevant attributes: 'inSubscenes' and 'flags'
  for (i in seq_along(objects_list)) {
    objects_list[[i]]$inSubscenes <- root
    objects_list[[i]]$flags <- numericFlags(getFlags(objects_list[[i]]))
  } 
  
  return(objects_list)
}