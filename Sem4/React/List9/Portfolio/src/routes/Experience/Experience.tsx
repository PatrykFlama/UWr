import modules from "./Experience.module.scss";
import {
    List,
    Divider,
} from "@mui/material";
import SchoolIcon from '@mui/icons-material/School';
import ArrowForwardIosIcon from '@mui/icons-material/ArrowForwardIos';
import { ExperienceField } from "./ExperienceField/ExperienceField";
import { useTheme } from "@mui/material/styles";

const experiences = [
    {
        icon: <SchoolIcon />,
        title: "University of Wrocław",
        description: "Computer Science",
        time_from: "2022 Oct",
        time_to: "Present\n(2025 Jun)",
    },
    {
        icon: <SchoolIcon />,
        title: "University of Wrocław",
        description: "Joint Studies in Computer Science and Mathemat",
        time_from: "2022 Oct",
        time_to: "2024 Jun",
    },
    {
        icon: <SchoolIcon />,
        title: "XIV High School in Wrocław",
        description: "Focused on mathematics, physics and computer science\n(+ additional cp-like classes in algorithms and data structures)",
        time_from: "2022",
        time_to: "2019",
    },
];


export default function Experience() {
    const theme = useTheme();

    return (
        <div className={modules.experience}>
            <List
                sx={{
                    width: "50%",
                }}
            >
                {experiences.map((experience) => (
                    <>
                        <ExperienceField
                            MUIicon={experience.icon || <ArrowForwardIosIcon />}
                            title={experience.title}
                            description={experience.description}
                            time_from={experience.time_from}
                            time_to={experience.time_to}
                            flex_ratio={[1, 4]}
                        />
                        <Divider variant="inset" component="li" sx={{ borderColor: theme.palette.text.secondary }} />
                    </>
                ))}
            </List>
        </div>
    );
}
