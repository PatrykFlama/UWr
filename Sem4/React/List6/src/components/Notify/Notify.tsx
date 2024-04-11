import { Snackbar } from "@mui/material";

interface IProps {
    text: string;
    open: boolean;
    setOpen: (open: boolean) => void;
}

export function Notify({ open, setOpen, text }: IProps) {
    const handleClose = (
        event: React.SyntheticEvent | Event,
        reason?: string
    ) => {
        if (reason === "clickaway") {
            return;
        }

        setOpen(false);
    };

    return (
        <Snackbar
            open={open}
            autoHideDuration={6000}
            onClose={handleClose}
            message={text}
        />
    );
}
