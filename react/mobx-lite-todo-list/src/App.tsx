import React from 'react';
import './App.css';
import { observer, useObservable } from 'mobx-react-lite'
import { TodoList } from './components/TodoList';
import { Footer } from './components/Footer';
import { todos } from './dataSource';

const App: React.FC = observer(() => {

  const store = useObservable({
    todos,
    toggleTodo(index: number): void {
      store.todos[index].completed = !store.todos[index].completed
    },
    get remainingTodos(): number {
      return store.todos.filter(t => !t.completed).length;
    }
  })
  return (
   <div className="App">
     <h2>Another Boring Todo App</h2>
      <TodoList
        todos={store.todos}
        toggleTodo={store.toggleTodo}
      />
      <Footer
        remaining={store.remainingTodos}
        total={store.todos.length}
      />
   </div>
  )
})

export default App;
