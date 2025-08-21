// Make <details> expand when hovered, collapse when mouse leaves
document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll("details.expand").forEach((detail) => {
    detail.addEventListener("mouseenter", () => {
      detail.setAttribute("open", "");
    });
    detail.addEventListener("mouseleave", () => {
      detail.removeAttribute("open");
    });
  });
});
