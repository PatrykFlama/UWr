import "./ThemeWrapper.scss";
import React from "react";
import { useTheme } from "../../providers/Theme";

import { createTheme, ThemeProvider as MuiThemeProvider } from "@mui/material";
import { grey, blue } from '@mui/material/colors';


const muitheme = createTheme({
    palette: {
        background: {
            default: grey[100],
        },
        primary: {
            main: grey[900],
        },
        secondary: {
            main: blue[900],
        },
        text: {
            primary: grey[900],
            secondary: grey[500],
        },
    },
});

const muitheme_dark = createTheme({
    palette: {
        background: {
            default: grey[800],
        },
        primary: {
            main: grey[100],
        },
        secondary: {
            main: blue[100],
        },
        text: {
            primary: grey[100],
            secondary: grey[300],
        },
    },
});


export default function ThemeWrapper({ children }: { children: React.ReactNode }) {
    const { theme } = useTheme();

    return (
        <div className={`portfolio ${theme === 'dark' ? "dark-theme" : "light-theme"}`}>
            <MuiThemeProvider theme={theme === 'dark' ? muitheme_dark : muitheme}>
                {children}
            </MuiThemeProvider>
        </div>
    );
}