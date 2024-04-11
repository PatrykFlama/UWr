import {
    Dialog,
    DialogTitle,
    DialogContent,
    TextField,
    FormControlLabel,
    Checkbox,
    DialogActions,
    Button,
} from "@mui/material";

interface IProps {
    open: boolean;
    onClose: () => void;
    onAdd: (event: React.FormEvent<HTMLFormElement>) => void;
}

export default function AddForm({open, onClose, onAdd}: IProps) {
    return (
        <Dialog
            open={open}
            onClose={onClose}
            PaperProps={{
                component: "form",
                onSubmit: onAdd,
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
                    control={<Checkbox id="accessibility" color="primary" />}
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
                <Button onClick={onClose}>Cancel</Button>
                <Button type="submit">Add</Button>
            </DialogActions>
        </Dialog>
    );
}
