import { AppBar, Toolbar, Typography, IconButton } from "@mui/material";
import { AccountCircle, NoAccounts } from "@mui/icons-material";


interface IProps {
    auth: boolean;
    setAuth: (auth: boolean) => void;
}

export default function ({auth, setAuth} : IProps) {
    return (
        <AppBar position="sticky">
            <Toolbar>
                <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
                    Sklep
                </Typography>

                <div>
                    <IconButton
                        size="large"
                        aria-label="account of current user"
                        aria-controls="menu-appbar"
                        aria-haspopup="true"
                        onClick={() => setAuth(!auth)}
                        color="inherit"
                    >
                        {auth ? <AccountCircle /> : <NoAccounts />}
                    </IconButton>
                </div>
            </Toolbar>
        </AppBar>
    );
}
