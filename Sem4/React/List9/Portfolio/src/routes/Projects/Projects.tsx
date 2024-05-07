import { Grid, Card, CardContent, Typography } from '@mui/material';


function Projects() {
    return (
        <div>
            <Grid container spacing={2}>
                {/* Replace the following code with your project data */}
                <Grid item xs={12} sm={6} md={4}>
                    <Card>
                        <CardContent>
                            <Typography variant="h5" component="h2">
                                Project 1
                            </Typography>
                            <Typography variant="body2" color="textSecondary" component="p">
                                Description of Project 1
                            </Typography>
                        </CardContent>
                    </Card>
                </Grid>
                <Grid item xs={12} sm={6} md={4}>
                    <Card>
                        <CardContent>
                            <Typography variant="h5" component="h2">
                                Project 2
                            </Typography>
                            <Typography variant="body2" color="textSecondary" component="p">
                                Description of Project 2
                            </Typography>
                        </CardContent>
                    </Card>
                </Grid>
                {/* Add more Grid items for additional projects */}
            </Grid>
        </div>
    );
};

export default Projects;