import modules from './App.module.scss'
import { Header } from './components/Header/Header'

function App() {
  return (
    <>
        <Header 
          title='Patryk Flama'
          content={[
            { title: 'Home', ref: '#home' },
            {
                title: 'About',
                ref: '#about',
                subMenu: [
                    { title: 'Me', ref: '#me' },
                    { title: 'You', ref: '#you' },
                ],
            },
            { title: 'Contact', ref: '#contact' },
        ]} />
    </>
  )
}

export default App
