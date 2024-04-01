import React from "react";
import { useTheme } from "../providers/Theme";


export default function ThemeWrapper({ children }: { children: React.ReactNode }) {
    const { theme } = useTheme();

    return (
        <div className={`portfolio ${theme === 'dark' ? "dark-theme" : "light-theme"}`}>
            {children}
        </div>
    );
}