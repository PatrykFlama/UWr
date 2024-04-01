import classes from "./Navbar.module.scss";
import { useTheme } from "../../providers/Theme";


export default function Navbar({ elements }: { elements: string[] }) {
    const { theme, toggleTheme } = useTheme();

    return (
        <div className={classes["navbar"]}>
            {elements.map((element) => (
                <a href={`#${element.toLowerCase()}`}>{element}</a>
            ))}

            <button onClick={toggleTheme} className={classes["theme-toggle-button"]}>
                {theme === 'light' ? "Light Mode" : "Dark Mode"}
            </button>
        </div>
    );
}


/*
      <div className="navbar">
        <a href="#header">Home</a>
        <a href="#about">About</a>
        <a href="#services">Services</a>
        <a href="#team">Team</a>
        <a href="#blog">Blog</a>
        <a href="#contact">Contact</a>
        <button onClick={toggleTheme} className="theme-toggle-button">
          {darkMode ? "Light Mode" : "Dark Mode"}
        </button>
      </div>
*/