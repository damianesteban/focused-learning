import Todo from './interfaces/Todo';
import * as uuid from 'node-uuid';

export const todos: Todo[] = [
  { id: uuid.v4(), text: 'Learn Mobx', completed: false },
  { id: uuid.v4(), text: 'Learn Haskell', completed: false },
  { id: uuid.v4(), text: 'Read The Pragmatic Programmer', completed: true },
]
