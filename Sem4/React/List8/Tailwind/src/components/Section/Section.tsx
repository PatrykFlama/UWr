interface IProps {
    id: string;
    header?: string;
    children: React.ReactNode;
}

export default function Section({ id, header, children }: IProps) {
    return (
        <section id={id} className={`py-5 even:bg-red-100 dark:even:bg-red-300`}>
            {header ? 
                <h2 className='text-2xl mb-5 inline-block' >{header}</h2>
                : null}
                
            <div className={"max-w-3xl mx-auto"}>
                {children}
            </div>
        </section>
    );
}
