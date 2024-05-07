import modules from "./RootLayout.module.scss";
import { NavLink, Outlet, useLocation, useNavigate } from "react-router-dom";
import { AppBar, Toolbar, Typography, Button } from "@mui/material";
import { Header } from "../../components/Header/Header";
import { Box, Container } from "@mui/system";

export default function RootLayout() {
    const location = useLocation();
    const navigate = useNavigate();

    return (
        <div className={modules.root}>
            <Header 
                navigation={[
                    { label: "About", ref: "/about" },
                    { label: "Contact", ref: "/contact" },
                    { label: "Projects", ref: "/projects" },
                ]}
            />

            <Outlet />

            <Box sx={{ p: 3, mt: 'auto' }}>
                <Container maxWidth="sm">
                    <Typography variant="body2" align="center">
                        {'© '}
                        {new Date().getFullYear()}
                        {' Patryk Flama'}
                    </Typography>
                </Container>
            </Box>
        </div>
    );
}
