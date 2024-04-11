import {
    Button,
    Dialog,
    DialogActions,
    DialogContent,
    DialogTitle,
    Typography,
} from "@mui/material";

interface IProps {
    open: boolean;
    onDelete: (event: React.FormEvent<HTMLFormElement>) => void;
    onClose: () => void;
}

export default function DeleteAlert({ open, onDelete, onClose }: IProps) {
    return (
        <Dialog
            open={open}
            onClose={onClose}
            PaperProps={{
                component: "form",
                onSubmit: onDelete,
            }}
        >
            <DialogTitle>Usuń produkt</DialogTitle>
            <DialogContent>
                <Typography>Czy na pewno chcesz usunąć ten produkt?</Typography>
            </DialogContent>
            <DialogActions>
                <Button onClick={onClose}>Anuluj</Button>
                <Button type="submit">Usuń</Button>
            </DialogActions>
        </Dialog>
    );
}
