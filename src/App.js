import React from 'react';
import logo from './logo.svg';
import './App.css';
import Counter from './components/Counter';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <Counter label="Click me!" />
      </header>
    </div>
  );
}

export default App;
