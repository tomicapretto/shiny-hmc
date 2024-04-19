
window.addEventListener("load", function() {
  const sidebar = document.getElementById("sidebar");
  
  let isResizing = false;
  let startX;
  
  sidebar.addEventListener("mousedown", (event) => {
    if (event.target === sidebar) {
      isResizing = true;
      startX = event.clientX;
      document.addEventListener("mousemove", handleMouseMove);
      document.addEventListener("mouseup", () => {
        isResizing = false;
        document.removeEventListener("mousemove", handleMouseMove);
      });
    }
  });
  
  function handleMouseMove(event) {
    if (isResizing) {
      const newWidth = event.clientX;
      sidebar.style.width = `${newWidth}px`;
    }
  }
});

