import {
    ListItem,
    ListItemText,
    ListItemAvatar,
    Avatar,
    Typography,
    Box,
} from "@mui/material";
import KeyboardArrowUpIcon from "@mui/icons-material/KeyboardArrowUp";
import { useTheme } from "@mui/material/styles";

export interface IExperienceField {
    MUIicon: JSX.Element;
    title: string;
    description: string;
    time_from?: string;
    time_to?: string;
    flex_ratio?: [number, number];
}

export function ExperienceField({
    MUIicon,
    title,
    description,
    time_from,
    time_to,
    flex_ratio,
}: IExperienceField) {
    const theme = useTheme();
    if (!flex_ratio) {
        flex_ratio = [1, 5];
    }

    return (
        <ListItem alignItems="center">
            <Box
                display="flex"
                alignItems="center"
                flexDirection="column"
                flex={flex_ratio[0]}
                minWidth="35px"
                style={{ marginRight: "8px" }}
            >
                {time_to ? (
                    <Typography variant="body2" color="text.secondary">
                        {time_to.split("\n").map((line, index) => (
                            <Typography
                                variant={index === 0 ? "body2" : "caption"}
                                color="text.secondary"
                            >
                                {line}
                                <br />
                            </Typography>
                        ))}
                    </Typography>
                ) : null}

                {time_to && time_from && time_to !== time_from ? (
                    <KeyboardArrowUpIcon
                        style={{ color: theme.palette.text.secondary }}
                    />
                ) : null}

                {time_to && time_from && time_to !== time_from ? (
                    <Typography variant="body2" color="text.secondary">
                        {time_from.split("\n").map((line, index) => (
                            <Typography
                                variant={index === 0 ? "body2" : "caption"}
                                color="text.secondary"
                            >
                                {line}
                                <br />
                            </Typography>
                        ))}
                    </Typography>
                ) : null}
            </Box>

            <ListItemAvatar>
                <Box display="flex" alignItems="center">
                    <Box display="flex" alignItems="center">
                        <Avatar>{MUIicon}</Avatar>
                    </Box>
                </Box>
            </ListItemAvatar>

            <Box flex={flex_ratio[1]}>
                <ListItemText
                    primary={title}
                    secondary={
                        <>
                            <Typography
                                component="span"
                                variant="body2"
                                color="text.primary"
                            >
                                {description.split("\n").map((line, index) => (
                                    <Typography
                                        variant={
                                            index === 0 ? "body2" : "caption"
                                        }
                                        color="text.secondary"
                                    >
                                        {line}
                                        <br />
                                    </Typography>
                                ))}
                            </Typography>
                        </>
                    }
                />
            </Box>
        </ListItem>
    );
}
