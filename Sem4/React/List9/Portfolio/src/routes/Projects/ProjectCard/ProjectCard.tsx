import {
    Card,
    CardContent,
    Typography,
    useTheme,
    CardActions,
    Button,
} from "@mui/material";

export interface IProjectCard {
    title: string;
    description: string;
    repo_label?: string;
    repo_link?: string;
    demo_label?: string;
    demo_link?: string;
}

export function ProjectCard({
    title,
    description,
    repo_label,
    repo_link,
    demo_label,
    demo_link,
}: IProjectCard) {
    const theme = useTheme();

    if (repo_link && !repo_label) repo_label = "Check out repository";
    if (demo_link && !demo_label) demo_label = "Demo";

    return (
        <Card sx={{ backgroundColor: theme.palette.background.default }}>
            <CardContent>
                <Typography variant="h5" color="text.primary" component="h2">
                    {title}
                </Typography>
                {description.split("\n").map((line, index) => (
                    <Typography
                        variant={index === 0 ? "body2" : "caption"}
                        color="text.secondary"
                    >
                        {line}
                        <br />
                    </Typography>
                ))}
            </CardContent>
            <CardActions>
                {repo_link && (
                    <Button size="small" color="primary" href={repo_link}>
                        {repo_label}
                    </Button>
                )}
                {demo_link && (
                    <Button size="small" color="primary" href={demo_link}>
                        {demo_label}
                    </Button>
                )}
            </CardActions>
        </Card>
    );
}
