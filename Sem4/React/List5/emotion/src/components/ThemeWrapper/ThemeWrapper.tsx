import React from "react";
import { ThemeProvider } from '@emotion/react';
import styled from '@emotion/styled';
import { useThemeMode } from "../../providers/ThemeMode";
import { lightTheme, darkTheme } from '../../_variables';

const Portfolio = styled.div`
  margin: 0 auto;
  background-color: ${props => props.theme.backgroundColor};
  color: ${props => props.theme.textColor};
`;

export default function ThemeWrapper({ children }: { children: React.ReactNode }) {
    const { theme } = useThemeMode();

    return (
        <ThemeProvider theme={theme === 'dark' ? darkTheme : lightTheme}>
            <Portfolio>
                {children}
            </Portfolio>
        </ThemeProvider>
    );
}