import * as React from "react";
import {
    Box,
    Button,
    AppBar,
    Toolbar,
    Typography,
    IconButton,
    CssBaseline,
    Fab,
    TextField,
    Dialog,
    DialogActions,
    DialogContent,
    DialogContentText,
    DialogTitle,
    Checkbox,
    FormControlLabel,
} from "@mui/material";
import {
    DataGrid,
    GridColDef,
    GridActionsCellItem,
    GridRowId,
} from "@mui/x-data-grid";
import DeleteIcon from "@mui/icons-material/DeleteOutlined";
import AddIcon from "@mui/icons-material/Add";
import AccountCircle from "@mui/icons-material/AccountCircle";
import NoAccountsIcon from "@mui/icons-material/NoAccounts";

const initialRows = [
    {
        id: 1,
        name: "Name1",
        type: "Type1",
        price: 100,
        accessibility: true,
        pcs: 10,
    },
    {
        id: 2,
        name: "Name2",
        type: "Type2",
        price: 200,
        accessibility: false,
        pcs: 20,
    },
    {
        id: 3,
        name: "Name3",
        type: "Type3",
        price: 300,
        accessibility: true,
        pcs: 30,
    },
    {
        id: 4,
        name: "Name4",
        type: "Type4",
        price: 400,
        accessibility: false,
        pcs: 40,
    },
    {
        id: 5,
        name: "Name5",
        type: "Type5",
        price: 500,
        accessibility: true,
        pcs: 50,
    },
    {
        id: 6,
        name: "Name6",
        type: "Type6",
        price: 600,
        accessibility: true,
        pcs: 60,
    },
    {
        id: 7,
        name: "Name7",
        type: "Type7",
        price: 700,
        accessibility: false,
        pcs: 70,
    },
    {
        id: 8,
        name: "Name8",
        type: "Type8",
        price: 800,
        accessibility: true,
        pcs: 80,
    },
    {
        id: 9,
        name: "Name9",
        type: "Type9",
        price: 900,
        accessibility: false,
        pcs: 90,
    },
    {
        id: 10,
        name: "Name10",
        type: "Type10",
        price: 1000,
        accessibility: true,
        pcs: 100,
    },
    {
        id: 11,
        name: "Name11",
        type: "Type11",
        price: 1100,
        accessibility: true,
        pcs: 110,
    },
    {
        id: 12,
        name: "Name12",
        type: "Type12",
        price: 1200,
        accessibility: false,
        pcs: 120,
    },
    {
        id: 13,
        name: "Name13",
        type: "Type13",
        price: 1300,
        accessibility: true,
        pcs: 130,
    },
    {
        id: 14,
        name: "Name14",
        type: "Type14",
        price: 1400,
        accessibility: false,
        pcs: 140,
    },
    {
        id: 15,
        name: "Name15",
        type: "Type15",
        price: 1500,
        accessibility: true,
        pcs: 150,
    },
];

export default function App() {
    const columns: GridColDef[] = [
        { field: "name", headerName: "Nazwa", width: 130 },
        { field: "type", headerName: "Typ", width: 70 },
        {
            field: "price",
            headerName: "Cena (PLN)",
            type: "number",
            width: 100,
        },
        {
            field: "accessibility",
            headerName: "Dostępność",
            type: "boolean",
            width: 100,
        },
        {
            field: "pcs",
            headerName: "Sztuki",
            type: "number",
            width: 100,
        },
        {
            field: "deleter",
            type: "actions",
            headerName: "Usuń",
            width: 70,
            cellClassName: "actions",
            sortable: false,
            getActions: ({ id }) => {
                return [
                    <GridActionsCellItem
                        icon={<DeleteIcon />}
                        label="Delete"
                        onClick={handleDeleteClick(id)}
                        color="inherit"
                    />,
                ];
            },
        },
    ];

    const [auth, setAuth] = React.useState(true);
    const [rows, setRows] = React.useState(initialRows);

    const handleDeleteClick = (id: GridRowId) => () => {
        setRows(rows.filter((row) => row.id !== id));
    };

    const [open, setOpen] = React.useState(false);

    const handleClickOpenForm = () => {
        setOpen(true);
    };

    const handleCloseForm = () => {
        setOpen(false);
    };

    const handleSubmitForm = (event: React.FormEvent) => {
        event.preventDefault();

        const form = event.currentTarget as HTMLFormElement;
        const name =
            (form.elements.namedItem("name") as HTMLInputElement).value ||
            "Default Name";
        const type =
            (form.elements.namedItem("type") as HTMLInputElement).value ||
            "Default Type";
        const price =
            parseInt(
                (form.elements.namedItem("price") as HTMLInputElement).value,
                10
            ) || 0;
        const accessibility = (
            form.elements.namedItem("accessibility") as HTMLInputElement
        ).checked;
        const pcs =
            parseInt(
                (form.elements.namedItem("pcs") as HTMLInputElement).value,
                10
            ) || 0;

        // debug
        console.log(name, type, price, accessibility, pcs);

        setRows([
            ...rows,
            { id: rows.length, name, type, price, accessibility, pcs },
        ]);

        handleCloseForm();
    };

    return (
        <>
            <CssBaseline />
            <Box sx={{ flexGrow: 1 }}>
                <AppBar position="sticky">
                    <Toolbar>
                        <Typography
                            variant="h6"
                            component="div"
                            sx={{ flexGrow: 1 }}
                        >
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
                                {auth ? <AccountCircle /> : <NoAccountsIcon />}
                            </IconButton>
                        </div>
                    </Toolbar>
                </AppBar>

                <DataGrid
                    rows={rows}
                    columns={columns}
                    initialState={{
                        pagination: {
                            paginationModel: { page: 0, pageSize: 5 },
                        },
                    }}
                    pageSizeOptions={[5, 10]}
                    sx={{
                        height: 400,
                        maxWidth: "60%",
                    }}
                />

                <Fab
                    color="primary"
                    aria-label="add"
                    onClick={handleClickOpenForm}
                    sx={{ position: "absolute", bottom: "20px", right: "20px" }}
                >
                    <AddIcon />
                </Fab>


                <Dialog
                    open={open}
                    onClose={handleCloseForm}
                    onSubmit={handleSubmitForm}
                    PaperProps={{
                        component: "form",
                        onSubmit: (event: React.FormEvent<HTMLFormElement>) => {
                            event.preventDefault();
                            const formData = new FormData(event.currentTarget);
                            const formJson = Object.fromEntries(
                                (formData as any).entries()
                            );
                            const email = formJson.email;
                            console.log(email);
                            handleCloseForm();
                        },
                    }}
                >
                    <DialogTitle>Dodaj produkt</DialogTitle>
                    <DialogContent>
                        <TextField
                            margin="dense"
                            id="name"
                            label="Nazwa"
                            type="text"
                            fullWidth
                        />

                        <TextField
                            margin="dense"
                            id="type"
                            label="Typ"
                            type="text"
                            fullWidth
                        />

                        <TextField
                            margin="dense"
                            id="price"
                            label="Cena"
                            type="number"
                            fullWidth
                        />

                        <FormControlLabel
                            control={
                                <Checkbox id="accessibility" color="primary" />
                            }
                            label="Dostępność"
                        />

                        <TextField
                            margin="dense"
                            id="pcs"
                            label="Pieces"
                            type="number"
                            fullWidth
                        />
                    </DialogContent>
                    <DialogActions>
                        <Button onClick={handleCloseForm}>Cancel</Button>
                        <Button type="submit">Subscribe</Button>
                    </DialogActions>
                </Dialog>
            </Box>
        </>
    );
}
