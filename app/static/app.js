
async function fetchTasks() {
  const res = await fetch('/tasks');
  const tasks = await res.json();
  const ul = document.getElementById('taskList');
  ul.innerHTML = '';
  tasks.forEach(t => {
    const li = document.createElement('li');
    li.textContent = t.title + (t.completed ? ' ✅' : '');
    ul.appendChild(li);
  });
}

async function addTask() {
  const title = document.getElementById('taskTitle').value;
  await fetch('/tasks', {
    method: 'POST',
    headers: {'Content-Type': 'application/x-www-form-urlencoded'},
    body: `title=${encodeURIComponent(title)}`
  });
  document.getElementById('taskTitle').value = '';
  fetchTasks();
}

window.onload = fetchTasks;
