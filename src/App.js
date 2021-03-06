import React from 'react';
import logo from './logo.svg';
import './App.css';
import { jsCounter as Counter } from './Components/Counter/Interop';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <Counter />
        <Counter onClick={n => console.log("clicked: ", n)} />
        <Counter counterType="decrementer" label="Try it out!" />
      </header>
    </div>
  );
}

export default App;
