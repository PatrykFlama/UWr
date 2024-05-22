import "./App.css";
import React, { useState } from "react";
import { AppBar, Toolbar, Typography, Button, Container } from "@mui/material";
import BookTable from "./components/BookTable";
import BookForm from "./components/BookForm";

function App() {
    const [open, setOpen] = useState(false);

    const handleOpen = () => setOpen(true);
    const handleClose = () => setOpen(false);

    return (
        <div>
            <AppBar position="static">
                <Toolbar>
                    <Typography variant="h6">Księgarnia</Typography>
                </Toolbar>
            </AppBar>

            <Container>
                <Button
                    onClick={handleOpen}
                    variant="contained"
                    color="primary"
                    sx={{ marginTop: 2 }}
                >
                    Dodaj książkę
                </Button>
                <BookTable />
                <BookForm open={open} handleClose={handleClose} />
            </Container>
        </div>
    );
}

export default App;
