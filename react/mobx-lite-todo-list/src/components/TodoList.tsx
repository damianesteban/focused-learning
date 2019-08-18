import React from 'react';
import { todos } from '../dataSource';
import Todo from '../interfaces/Todo';

interface Props {
  todos: Todo[];
  toggleTodo: (index: number) => void
}

export const TodoList: React.FC<Props> = props => {
  return (
    <ul
      data-testid='todoList'
      style={{ listStyle: 'none '}}
    >
      {props.todos &&
        props.todos.map((todo, index) => (
          <li
            onClick={ () => props.toggleTodo(index)}
            style= {{ 
              margin: 10,
              opacity: todo.completed ? 0.5 : 1,
              cursor: 'pointer',
              textDecoration: todo.completed ? 'line-through' : 'none'
            }}
            key={todo.id}
          >
            {todo.text}
          </li>
        ))}
    </ul>
  )
}
