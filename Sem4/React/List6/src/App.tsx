import * as React from "react";
import {
    Box,
    CssBaseline,
    Fab,
    ThemeProvider,
} from "@mui/material";
import {
    DataGrid,
    GridColDef,
    GridActionsCellItem,
    GridRowId,
} from "@mui/x-data-grid";
import DeleteIcon from "@mui/icons-material/DeleteOutlined";
import AddIcon from "@mui/icons-material/Add";

import DeleteAlert from "./components/DeleteAlert/DeleteAlert";
import AddForm from "./components/AddForm/AddForm";
import { initialRows } from "./components/DataTable/initialRows";
import Header from "./components/Header/Header";
import { Notify } from "./components/Notify/Notify";
import theme from "./components/Theme/Theme";

export default function App() {
    const [auth, setAuth] = React.useState(true);

    // notify
    const [openNotify, setOpenNotify] = React.useState(false);
    const [textNotify, setTextNotify] = React.useState("");

    // form dialog
    const [openForm, setOpenForm] = React.useState(false);

    const handleOpenForm = () => {
        setOpenForm(true);
    };

    const handleCloseForm = () => {
        setOpenForm(false);
    };

    const handleSubmitForm = (event: React.FormEvent) => {
        event.preventDefault();

        const form = event.target as any;
        const newRow = {
            id: Math.random(),
            name: form.name.value as string,
            type: form.type.value as string,
            price: form.price.value as number,
            accessibility: form.accessibility.value as boolean,
            pcs: form.pcs.value as number,
        };

        // validate
        if (newRow.name == "" || newRow.type == "") {
            setTextNotify("Name and type cant be empty!");
            setOpenNotify(true);
            return;
        }

        setRows([...rows, newRow]);

        handleCloseForm();

        setTextNotify("Success!");
        setOpenNotify(true);
    };

    // data grid
    const [rows, setRows] = React.useState(initialRows);

    const deleteGetAction = ({ id }: { id: GridRowId }) => {
        return [
            <GridActionsCellItem
                icon={<DeleteIcon />}
                label="Delete"
                onClick={handleDeleteClick(id)}
                color="inherit"
            />,
        ];
    };

    const columns: GridColDef[] = [
        { field: "name", headerName: "Nazwa", width: 130 },
        { field: "type", headerName: "Typ", width: 70 },
        {
            field: "price",
            headerName: "Cena (PLN)",
            type: "number",
            width: 100,
            renderCell: (params) => {
                return params.value + " PLN";
            },
        },
        {
            field: "accessibility",
            headerName: "Dostępność",
            type: "boolean",
            width: 100,
            renderCell: (params) => {
                return params.value ? "Tak" : "Nie";
            },
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
            getActions: deleteGetAction,
        },
    ];

    // delete button
    const [deleteId, setDeleteId] = React.useState<GridRowId>(0);

    const handleDeleteClick = (id: GridRowId) => () => {
        setDeleteId(id);
        handleOpenDeleteWarning();
    };

    // delete alert dialog
    const [openDeleteAlert, setOpenDeleteAlert] = React.useState(false);

    const handleOpenDeleteWarning = () => {
        setOpenDeleteAlert(true);
    };

    const handleCloseDeleteWarning = () => {
        setOpenDeleteAlert(false);
    };

    return (
        <>
            <CssBaseline />
            <ThemeProvider theme={theme}>
                <Box sx={{ flexGrow: 1 }}>
                    <Header auth={auth} setAuth={setAuth} />

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
                        onClick={handleOpenForm}
                        sx={{
                            position: "absolute",
                            bottom: "20px",
                            right: "20px",
                        }}
                    >
                        <AddIcon />
                    </Fab>

                    <AddForm
                        open={openForm}
                        onClose={handleCloseForm}
                        onAdd={handleSubmitForm}
                    />

                    <DeleteAlert
                        open={openDeleteAlert}
                        onDelete={(event: React.FormEvent<HTMLFormElement>) => {
                            event.preventDefault();
                            setRows(rows.filter((row) => row.id !== deleteId));
                            handleCloseDeleteWarning();
                        }}
                        onClose={handleCloseDeleteWarning}
                    />

                    <Notify
                        open={openNotify}
                        setOpen={setOpenNotify}
                        text={textNotify}
                    />
                </Box>
            </ThemeProvider>
        </>
    );
}
