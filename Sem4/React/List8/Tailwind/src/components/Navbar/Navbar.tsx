import { useTheme } from "../../providers/Theme";

export default function Navbar({ elements }: { elements: string[] }) {
    const { theme, toggleTheme } = useTheme();

    const buttonClasses = `
        cursor-pointer
        py-2 px-4
        transition-colors duration-300 ease-in-out
        bg-gray-800
        text-white
        rounded-lg
        cursor-pointer
        transition-colors duration-300 ease-in-out      
        `;

    const hoverClass = `hover:bg-hoverColor`;

    const darkButtonClasses = `
        py-2 px-4 
        rounded 
        dark:bg-gray-200
        dark:text-gray-700
        dark:hover:bg-gray-400
        cursor-pointer
        transition
        duration-300
        ease-in-out
    `;
    const darkHoverClass = `dark:hover:bg-gray-contactFormTextarea`;

    const lightNavbarClasses = {
        navbar: "sticky top-0 px-4 py-2 text-center bg-gray-300 text-gray-700",
        link: "text-gray-700 px-4 py-2 hover:text-gray-900",
    };

    const darkNavbarClasses = {
        navbar: "sticky top-0 px-4 py-2 text-center bg-gray-800 text-white dark:bg-gray-800 dark:text-white",
        link: "text-gray-300 px-4 py-2 hover:text-gray-400 dark:text-gray-300 dark:hover:text-gray-400",
    };


    return (
        <div className={`${lightNavbarClasses.navbar} ${darkNavbarClasses.navbar}`}>
            {elements.map((element) => (
                <a 
                    className={`${lightNavbarClasses.link} ${darkNavbarClasses.link}`}
                    href={`#${element.toLowerCase()}`}
                >{element}</a>
            ))}

            <button
                onClick={toggleTheme}
                className={`${buttonClasses} ${hoverClass} ${darkButtonClasses} ${darkHoverClass}`}
            >
                {theme === "light" ? "Light Mode" : "Dark Mode"}
            </button>
        </div>
    );
}
