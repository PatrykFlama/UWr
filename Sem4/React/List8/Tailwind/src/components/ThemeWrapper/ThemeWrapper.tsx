import React from "react";
import { useTheme } from "../../providers/Theme";


export default function ThemeWrapper({ children }: { children: React.ReactNode }) {
    const { theme } = useTheme();

    return (
        <div className={`mx-auto ${theme === 'dark' ? "dark" : ""} bg-white-100 dark:bg-black text-black dark:text-white`}>
            {children}
        </div>
    );
}