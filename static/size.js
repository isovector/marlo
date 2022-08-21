function saveWindowSize() {
  document.cookie = "size=" + window.innerWidth + "," + window.innerHeight + ";path=/";

}

window.onload = saveWindowSize;
window.onresize = saveWindowSize;
