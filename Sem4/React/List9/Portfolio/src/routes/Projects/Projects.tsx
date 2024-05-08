import { Grid, Box } from '@mui/material';
import { ProjectCard, IProjectCard } from './ProjectCard/ProjectCard';

const projects : IProjectCard[] = [
    {
        title: 'Fajrantinator.pl',
        description: 'Express.js based simple online shop (JavaScript)',
        repo_link: 'https://github.com/PatrykFlama/FajrantInator.pl',
        demo_link: 'https://fajrantinator.patrykflama.com',
    },
    {
        title: 'Library system',
        description: 'Program for managing university library (Java)',
        repo_link: 'https://github.com/PatrykFlama/LibrarySystem',
    },
    {
        title: 'Algorithms',
        description: 'Short documentation of some algorithms, written in polish',
        repo_link: 'https://github.com/PatrykFlama/Algorytmy',
    },
    {
        title: 'Themis',
        description: 'Programms from CP-focused classes taken at XIV high school in Wrocław',
        repo_link: 'https://github.com/PatrykFlama/Themis',
    },
    {
        title: 'UWr',
        description: 'Repository for some of the courses I took at the University of Wrocław',
        repo_link: 'https://github.com/PatrykFlama/UWr',
    },
    {
        title: 'Mini projects',
        description: 'Projects/miniprojects that are to small to have dedicated repository',
        repo_link: 'https://github.com/PatrykFlama/MiniProjects',
    },
];



export default function Projects() {
    return (
        <Box display="flex" justifyContent="center">
            <Grid container spacing={2} width="80%">
                {projects.map((project) => (
                    <Grid item xs={12} sm={6} md={4}>
                        <ProjectCard 
                            title={project.title ? project.title : ""} 
                            description={project.description ? project.description : ""} 
                            repo_link={project.repo_link ? project.repo_link : undefined}
                            demo_link={project.demo_link ? project.demo_link : undefined}
                        />
                    </Grid>
                ))}
            </Grid>
        </Box>
    );
}